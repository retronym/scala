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

package scala.reflect.internal.util;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

/**
 * async-profiler's spans (`one.profiler.Span` of retronym/async-profiler's integration branch), for the x-ray
 * (`ParallelTyperTimeline`), without a dependency on its jar: without the class, or without a running JFR recording,
 * `start` answers 0 and the other methods do nothing. In Java for the `static final` handles, which the JIT inlines.
 *
 * Loaded on the first span of a recording. The profiler's library must be loaded by then (`-agentpath`), as
 * `one.profiler.Recording` asks for its natives once, when it is initialised.
 *
 * Tags are precomputed strings: a tag crosses JNI once per span and is looked up in the JFR's dictionary there.
 */
public final class ProfilerSpans {
    private static final MethodHandle START;
    private static final MethodHandle END;
    private static final MethodHandle END_IF_PROFILED;

    static {
        MethodHandle start = null, end = null, endIf = null;
        try {
            Class<?> span = Class.forName("one.profiler.Span", false, ProfilerSpans.class.getClassLoader());
            MethodHandles.Lookup l = MethodHandles.publicLookup();
            MethodType ender = MethodType.methodType(void.class, long.class, String.class);
            start = l.findStatic(span, "start", MethodType.methodType(long.class));
            end = l.findStatic(span, "end", ender);
            endIf = l.findStatic(span, "endIfProfiled", ender);
        } catch (ReflectiveOperationException | LinkageError e) {
            start = null;
        }
        START = start;
        END = end;
        END_IF_PROFILED = endIf;
    }

    private ProfilerSpans() {}

    /** The start of a span on this thread, on the profiler's clock, or 0 if no JFR recording is running. */
    public static long start() {
        if (START == null) return 0L;
        try {
            return (long) START.invokeExact();
        } catch (Throwable t) {
            return 0L;
        }
    }

    /** Ends a span from `start`. */
    public static void end(long start, String tag) {
        if (start != 0L) {
            try {
                END.invokeExact(start, tag);
            } catch (Throwable t) {
                // a profile without this span
            }
        }
    }

    /** Ends a span from `start`, recording it only if a sample was taken on this thread during it, without entering
     *  native code otherwise: for frequent spans. */
    public static void endIfProfiled(long start, String tag) {
        if (start != 0L) {
            try {
                END_IF_PROFILED.invokeExact(start, tag);
            } catch (Throwable t) {
                // a profile without this span
            }
        }
    }
}
