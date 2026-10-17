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
import java.lang.module.ModuleReader;
import java.lang.module.ModuleReference;
import java.net.URI;

final class FixedModuleReference extends ModuleReference {
    public FixedModuleReference(ModuleDescriptor descriptor, URI location) {
        super(descriptor, location);
    }

    @Override
    public ModuleReader open() throws IOException {
        return NoopModuleReader.INSTANCE;
    }

}
