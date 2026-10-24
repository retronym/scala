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

import java.io.IOException;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReader;
import java.lang.module.ModuleReference;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

final class ExportRequireAddingModuleFinder implements ModuleFinder {
    private ModuleFinder delegate;
    private ExportRequireAdder patcher;

    public ExportRequireAddingModuleFinder(ModuleFinder delegate, ExportRequireAdder adder) {
        this.delegate = delegate;
        this.patcher = adder;
    }

    @Override
    public Optional<ModuleReference> find(String name) {
        return delegate.find(name).map(this::patch);
    }

    @Override
    public Set<ModuleReference> findAll() {
        return delegate.findAll().stream().map(this::patch).collect(Collectors.toSet());
    }

    private ModuleReference patch(ModuleReference delegate) {
        ModuleDescriptor delegateDesc = delegate.descriptor();
        ModuleDescriptor patch = patcher.patch(delegateDesc);
        if (delegateDesc == patch) {
            return delegate;
        } else {
            return new ModuleReference(patch, delegate.location().orElse(null)) {
                @Override
                public ModuleReader open() throws IOException {
                    return delegate.open();
                }
            };
        }
    }
}
