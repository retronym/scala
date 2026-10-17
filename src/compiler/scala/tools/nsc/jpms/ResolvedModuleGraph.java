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

    /** The module of a source file being compiled: the module it is patched into, or the current module. */
    public String moduleForSourceFile(Path path) {
        if (path != null && patchModuleOfSource != null) {
            String patched = patchModuleOfSource.apply(path);
            if (patched != null) return patched;
        }
        return currentModule;
    }
}
