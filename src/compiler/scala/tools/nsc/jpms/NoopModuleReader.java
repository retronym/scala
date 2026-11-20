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
import java.io.InputStream;
import java.lang.module.ModuleReader;
import java.net.URI;
import java.nio.ByteBuffer;
import java.util.Optional;
import java.util.stream.Stream;

class NoopModuleReader implements ModuleReader {
    static final ModuleReader INSTANCE = new NoopModuleReader();

    @Override
    public Optional<URI> find(String name) throws IOException {
        return Optional.empty();
    }

    @Override
    public Optional<InputStream> open(String name) throws IOException {
        return Optional.empty();
    }

    @Override
    public Optional<ByteBuffer> read(String name) throws IOException {
        return Optional.empty();
    }

    @Override
    public void release(ByteBuffer bb) {
    }

    @Override
    public Stream<String> list() throws IOException {
        return Stream.empty();
    }

    @Override
    public void close() throws IOException {
    }
}
