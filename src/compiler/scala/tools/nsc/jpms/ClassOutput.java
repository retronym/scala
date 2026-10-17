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

import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Set;
import java.util.function.Supplier;

public abstract class ClassOutput {
    public void configure(StandardJavaFileManager fileManager) {
    }

    /** A finder for the `module-info.class` in the class output, or null if there is none. */
    public abstract ModuleFinder moduleFinder() throws IOException;

    public static class OutputPathClassOutput extends ClassOutput {
        private Path path;

        public OutputPathClassOutput(Path path) {
            this.path = path;
        }

        @Override
        public void configure(StandardJavaFileManager fileManager) {
            try {
                fileManager.setLocationFromPaths(StandardLocation.CLASS_OUTPUT, Collections.singleton(path));
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            }
        }

        @Override
        public ModuleFinder moduleFinder() throws IOException {
            if (Files.isRegularFile(path.resolve("module-info.class"))) {
                ModuleFinder classOutputModuleFinder = ModuleFinder.of(path);
                Set<ModuleReference> classOutputModules = classOutputModuleFinder.findAll();
                if (classOutputModules.size() != 1) {
                    throw new IllegalStateException("Expected one module-info.class in " + path);
                }
                return classOutputModuleFinder;
            } else return null;
        }
    }

    public static class SupplierClassOutput extends ClassOutput {
        private Supplier<byte[]> supplier;

        public SupplierClassOutput(Supplier<byte[]> supplier) {
            this.supplier = supplier;
        }

        @Override
        public ModuleFinder moduleFinder() throws IOException {
            byte[] contents = supplier.get();
            if (contents == null) return null;
            // Annoyingly, I can't find a way to load a module-info.class file with `ModuleFinder.of`
            // without writing into to a NIO Path directory.
            // TODO JPMS This cleanup up this temp directory.
            Path tempDirectory = Files.createTempDirectory("suppler-class-output-");
            Files.write(tempDirectory.resolve("module-info.class"), contents);
            return ModuleFinder.of(tempDirectory);
        }
    }
}
