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

import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.module.ModuleFinder;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Consumer;

/** javac's file manager, configured with javac's module options, for use by `JpmsClassPath`. */
public class JpmsClasspathSupport {
    private final StandardJavaFileManager fileManager;
    private final ModuleGraphResolver resolver;
    private final Optional<String> release;

    public JpmsClasspathSupport(Optional<String> release, ClassOutput classOutput, List<List<String>> fileManagerOptions, List<String> addModules,
                                List<String> addExports, List<String> addReads) throws IOException {
        this.release = release;
        List<List<String>> unhandled = new ArrayList<>();
        Consumer<StandardJavaFileManager> c = (fm -> {
            classOutput.configure(fm);
            for (List<String> optionSubList: fileManagerOptions) {
                Iterator<String> iterator = optionSubList.iterator();
                while (iterator.hasNext()) {
                    String next = iterator.next();
                    boolean handled = fm.handleOption(next, iterator);
                    if (!handled) {
                        unhandled.add(optionSubList);
                        break;
                    }
                }
            }
        });
        ModuleFinderAndFileManager moduleFinderAndFileManager = ModuleFinderAndFileManager.get(release, c);
        fileManager = moduleFinderAndFileManager.fileManager();
        if (!unhandled.isEmpty()) throw new IllegalArgumentException("Some options were unhandled: " + unhandled);

        ModuleFinder modulePathFinder = ModuleFinderAndFileManager.locationToFinder(fileManager, StandardLocation.MODULE_PATH);
        resolver = new ModuleGraphResolver(moduleFinderAndFileManager.moduleFinder(), modulePathFinder, classOutput,
                addModules, addExports, addReads, this::patchModuleOfSource);
    }

    public ResolvedModuleGraph resolveModuleGraph(JpmsModuleDescriptor sourceModule) throws IOException {
        return resolver.resolve(sourceModule);
    }

    /** The module a source file is patched into with `--patch-module`, according to the file manager, or null. */
    private String patchModuleOfSource(Path path) {
        try {
            if (!fileManager.hasLocation(StandardLocation.PATCH_MODULE_PATH)) return null;
            JavaFileObject sourceFile = fileManager.getJavaFileObjects(path).iterator().next();
            for (Set<JavaFileManager.Location> set : fileManager.listLocationsForModules(StandardLocation.PATCH_MODULE_PATH)) {
                for (JavaFileManager.Location location : set) {
                    if (fileManager.contains(location, sourceFile)) return fileManager.inferModuleName(location);
                }
            }
            return null;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public StandardJavaFileManager getFileManager() {
        return fileManager;
    }

    public Optional<String> getRelease() {
        return release;
    }
}
