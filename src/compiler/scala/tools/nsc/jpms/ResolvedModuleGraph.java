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

import java.lang.module.Configuration;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ResolvedModule;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;

import static scala.tools.nsc.jpms.ModuleGraphResolver.UNNAMED_MODULE_NAME;

/** A resolved module graph, and the module of the sources being compiled. */
public final class ResolvedModuleGraph {
    /** Marks a module that exports all of its packages: an automatic module, or the unnamed module. */
    public static final Set<String> EXPORT_ALL = Collections.unmodifiableSet(new HashSet<>());

    private final String currentModule;
    private final Configuration configuration;
    private final Function<Path, String> patchModuleOfSource;

    ResolvedModuleGraph(String currentModule, Configuration configuration, Function<Path, String> patchModuleOfSource) {
        this.currentModule = currentModule;
        this.configuration = configuration;
        this.patchModuleOfSource = patchModuleOfSource;
    }

    /** For each module read by `siteModuleName`, the packages it exports to `siteModuleName`, or `EXPORT_ALL`. */
    public Map<String, Set<String>> accessibleModulePackages(String siteModuleName) {
        Set<ResolvedModule> readModules;
        String finalSiteModuleName = siteModuleName;
        if (siteModuleName.equals("")) {
            // The unnamed module reads every module in the graph.
            readModules = configuration.modules();
            finalSiteModuleName = UNNAMED_MODULE_NAME;
        } else {
            readModules = configuration.findModule(siteModuleName).map(ResolvedModule::reads).orElse(Collections.emptySet());
        }

        HashMap<String, Set<String>> result = new HashMap<>();
        for (ResolvedModule readModule : readModules) {
            ModuleDescriptor descriptor = readModule.reference().descriptor();
            // The unnamed module exports all of its packages (JLS 7.7.5), as does an automatic module.
            if (descriptor.isAutomatic() || readModule.name().equals(UNNAMED_MODULE_NAME)) {
                result.put(readModule.name(), EXPORT_ALL);
            } else {
                HashSet<String> packages = new HashSet<>();
                result.put(readModule.name(), packages);
                for (ModuleDescriptor.Exports exports : descriptor.exports()) {
                    if (!exports.isQualified() || exports.targets().contains(finalSiteModuleName)) {
                        packages.add(exports.source());
                    }
                }
            }
        }
        return result;
    }

    /**
     * Why `pkg` of module `target` is not accessible from module `from`, in the words javac uses, followed by the
     * options or directive that would make it accessible. The modules are named as in `accessibleModulePackages`.
     */
    public String explainInaccessible(String from, String target, String pkg) {
        String fromName = from.isEmpty() ? "the unnamed module" : "module " + from;
        String targetName = target.isEmpty() ? "the unnamed module" : "module " + target;
        String graphTarget = target.isEmpty() ? UNNAMED_MODULE_NAME : target;
        String graphFrom = from.isEmpty() ? UNNAMED_MODULE_NAME : from;
        Optional<ResolvedModule> resolvedTarget = configuration.findModule(graphTarget);
        if (resolvedTarget.isEmpty()) {
            return "package " + pkg + " is declared in " + targetName + ", which is not in the module graph\n" +
                (from.isEmpty() ? "Use --add-modules " + target + " to resolve it" : "Add `requires " + target + ";` to the declaration of " + fromName + ", or use --add-modules " + target);
        }
        boolean reads = from.isEmpty() || configuration.findModule(graphFrom).map(m -> m.reads().contains(resolvedTarget.get())).orElse(false);
        if (!reads) {
            String readsOption = "--add-reads " + from + "=" + (target.isEmpty() ? "ALL-UNNAMED" : target);
            return "package " + pkg + " is declared in " + targetName + ", but " + fromName + " does not read it\n" +
                (target.isEmpty() ? "Use " + readsOption : "Add `requires " + target + ";` to the declaration of " + fromName + ", or use " + readsOption);
        }
        boolean exportedToSomeone = resolvedTarget.get().reference().descriptor().exports().stream().anyMatch(e -> e.source().equals(pkg));
        String exportsOption = "--add-exports " + target + "/" + pkg + "=" + (from.isEmpty() ? "ALL-UNNAMED" : from);
        return "package " + pkg + " is declared in " + targetName + ", which does not export it" + (exportedToSomeone ? " to " + fromName : "") + "\n" +
            "Use " + exportsOption + " to export it anyway";
    }

    /** The module of the sources being compiled, other than those patched into another module; "" if unnamed. */
    public String currentModule() {
        return currentModule;
    }

    /** The module of a source file being compiled: the module it is patched into, or the current module. */
    public String moduleForSourceFile(Path path) {
        if (path != null && patchModuleOfSource != null) {
            String patched = patchModuleOfSource.apply(path);
            if (patched != null) return patched;
        }
        return currentModule;
    }
}
