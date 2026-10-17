/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.jpms;

import javax.lang.model.SourceVersion;
import java.io.IOException;
import java.lang.module.Configuration;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Resolves the module graph for a compilation run with `java.lang.module.Configuration`, applying
 * `--add-modules`, `--add-exports` and `--add-reads`. Independent of javac's file manager.
 */
public final class ModuleGraphResolver {
    // has to be a valid module name, so we can't use the pseudo-name "ALL-UNNAMED"
    public static final String UNNAMED_MODULE_NAME = "___UNNAMED";
    // ... which we do need when parsing command line options.
    private static final String ALL_UNNAMED = "ALL-UNNAMED";
    private static final String ALL_MODULE_PATH = "ALL-MODULE-PATH";
    private static final String ALL_SYSTEM = "ALL-SYSTEM";
    private static final String ALL_DEFAULT = "ALL-DEFAULT";

    private static final Pattern ADD_REQUIRES_PATTERN = Pattern.compile("([^=]+)=(,*[^,].*)");
    private static final Pattern ADD_EXPORTS_PATTERN = Pattern.compile("([^/]+)/([^=]+)=(,*[^,].*)");

    private final ModuleFinder systemFinder;
    private final ModuleFinder modulePathFinder;
    private final ClassOutput classOutput;
    private final List<String> addModules;
    private final ExportRequireAdder adder;
    private final Function<Path, String> patchModuleOfSource;

    /**
     * @param systemFinder        the system modules (including any upgrade module path)
     * @param modulePathFinder    the application module path
     * @param classOutput         the output directory, which may hold a `module-info.class` from an earlier compilation
     * @param patchModuleOfSource the module that a source file is patched into by `--patch-module`, or null
     */
    public ModuleGraphResolver(ModuleFinder systemFinder, ModuleFinder modulePathFinder, ClassOutput classOutput,
                               List<String> addModules, List<String> addExports, List<String> addReads,
                               Function<Path, String> patchModuleOfSource) {
        this.systemFinder = systemFinder;
        this.modulePathFinder = modulePathFinder;
        this.classOutput = classOutput;
        this.addModules = addModules;
        this.adder = exportRequireAdder(addExports, addReads);
        this.patchModuleOfSource = patchModuleOfSource;
    }

    public ResolvedModuleGraph resolve(JpmsModuleDescriptor sourceModule) throws IOException {
        ModuleDescriptor sourceModuleDescriptor = sourceModule == null ? null : sourceModule.create();
        List<ModuleDescriptor> fromSource = new ArrayList<>();
        if (sourceModuleDescriptor != null) fromSource.add(sourceModuleDescriptor);
        fromSource.add(ModuleDescriptor.newModule(UNNAMED_MODULE_NAME).requires("java.base").build());
        ModuleFinder finder = FixedModuleFinder.newModuleFinder(fromSource);

        String currentModule;
        Set<String> roots = new LinkedHashSet<>();
        if (sourceModuleDescriptor == null) {
            // No module-info.java among the sources. Consider a module-info.class in the output directory, from an
            // earlier compilation of the module's declaration.
            ModuleFinder classOutputModuleFinder = classOutput.moduleFinder();
            if (classOutputModuleFinder != null) {
                finder = ModuleFinder.compose(classOutputModuleFinder, finder);
                ModuleReference ref = classOutputModuleFinder.findAll().iterator().next();
                currentModule = ref.descriptor().name();
                roots.add(currentModule);
            } else {
                currentModule = "";
                roots.addAll(defaultRootModules());
            }
        } else {
            // The other sources being compiled are part of this module, except those under a `--patch-module` path.
            currentModule = sourceModuleDescriptor.name();
            roots.add(currentModule);
        }
        roots.add(UNNAMED_MODULE_NAME);
        for (String addModule : addModules) {
            switch (addModule) {
                case ALL_DEFAULT: roots.addAll(defaultRootModules()); break;
                case ALL_SYSTEM: systemFinder.findAll().forEach(r -> roots.add(r.descriptor().name())); break;
                case ALL_MODULE_PATH: modulePathFinder.findAll().forEach(r -> roots.add(r.descriptor().name())); break;
                default: roots.add(addModule);
            }
        }
        // The module(s) from source are passed as the `before` finder to take precedence over, rather than clash with,
        // a module-info.class in the output directory.
        ModuleFinder after = new ExportRequireAddingModuleFinder(ModuleFinder.compose(systemFinder, modulePathFinder), adder);
        Configuration configuration = Configuration.empty().resolve(new ExportRequireAddingModuleFinder(finder, adder), after, roots);
        return new ResolvedModuleGraph(currentModule, configuration, patchModuleOfSource);
    }

    /**
     * The default set of root modules when the main module is the unnamed module (JEP 261): every system module that
     * exports at least one package without qualification.
     *
     * TODO JPMS Incubator modules are excluded by name. The JDK marks them with the ModuleResolution attribute
     *      (DO_NOT_RESOLVE_BY_DEFAULT), which has no public API.
     */
    private Set<String> defaultRootModules() {
        Set<String> result = new LinkedHashSet<>();
        for (ModuleReference ref : systemFinder.findAll()) {
            ModuleDescriptor descriptor = ref.descriptor();
            if (descriptor.name().startsWith("jdk.incubator.")) continue;
            if (descriptor.exports().stream().anyMatch(e -> !e.isQualified())) result.add(descriptor.name());
        }
        return result;
    }

    private static ExportRequireAdder exportRequireAdder(List<String> addExports, List<String> addReads) {
        HashMap<String, List<ModuleDescriptor.Exports>> addExportsMap = processAddExports(addExports);
        HashMap<String, List<String>> addRequires = processAddReads(addReads);
        return new ExportRequireAdder() {
            @Override
            public Iterable<ModuleDescriptor.Exports> addExports(String moduleName) {
                return addExportsMap.getOrDefault(moduleName, Collections.emptyList());
            }

            @Override
            public Iterable<String> addReads(String moduleName) {
                return addRequires.getOrDefault(moduleName, Collections.emptyList());
            }
        };
    }

    private static HashMap<String, List<ModuleDescriptor.Exports>> processAddExports(List<String> addExports) {
        HashMap<String, List<ModuleDescriptor.Exports>> addExportsMap = new HashMap<>();
        for (String addExport : addExports) {
            Matcher matcher = ADD_EXPORTS_PATTERN.matcher(addExport);
            if (!matcher.matches())
                throw new IllegalArgumentException("Invalid --add-exports specification: " + addExport);
            String moduleName = matcher.group(1);
            if (!SourceVersion.isName(moduleName))
                throw new IllegalArgumentException("Invalid module name in " + addExport);
            String packageName = matcher.group(2);
            if (!SourceVersion.isName(packageName))
                throw new IllegalArgumentException("Invalid package name in " + addExport);
            HashSet<String> targets = new HashSet<>();
            for (String targetName : matcher.group(3).split(",")) {
                targets.add(targetName.equals(ALL_UNNAMED) ? UNNAMED_MODULE_NAME : targetName);
            }
            addExportsMap.computeIfAbsent(moduleName, k -> new ArrayList<>()).add(mkExports(packageName, targets));
        }
        return addExportsMap;
    }

    private static HashMap<String, List<String>> processAddReads(List<String> addReads) {
        HashMap<String, List<String>> result = new HashMap<>();
        for (String addRead : addReads) {
            Matcher matcher = ADD_REQUIRES_PATTERN.matcher(addRead);
            if (!matcher.matches())
                throw new IllegalArgumentException("Invalid --add-reads specification: " + addRead);
            String moduleName = matcher.group(1);
            if (!SourceVersion.isName(moduleName))
                throw new IllegalArgumentException("Invalid module name in " + addRead);
            List<String> requires = result.computeIfAbsent(moduleName, k -> new ArrayList<>());
            for (String required : matcher.group(2).split(",")) {
                requires.add(required.equals(ALL_UNNAMED) ? UNNAMED_MODULE_NAME : required);
            }
        }
        return result;
    }

    private static ModuleDescriptor.Exports mkExports(String packageName, Set<String> targets) {
        return ModuleDescriptor.newModule("dummy").exports(Set.of(), packageName, targets).build().exports().iterator().next();
    }
}
