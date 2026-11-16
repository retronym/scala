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

import java.util.concurrent.atomic.AtomicLong;

/** Spike: selects the implementation of `Types.unique` once per JVM.
 *
 *  `-Dscalac.uniques=weak|direct|off` (default `weak`).
 *  `-Dscalac.uniques.stats=true` counts hits and misses and prints them at JVM exit.
 */
public final class UniquesMode {
  public static final int WEAK = 0;
  public static final int DIRECT = 1;
  public static final int OFF = 2;

  public static final int MODE;
  public static final boolean STATS = Boolean.getBoolean("scalac.uniques.stats");
  public static final int DIRECT_BITS = Integer.getInteger("scalac.uniques.direct.bits", 14);
  public static final int DIRECT_REFRESH = Integer.getInteger("scalac.uniques.direct.refresh", 65536);

  public static final AtomicLong hits = new AtomicLong();
  public static final AtomicLong misses = new AtomicLong();
  public static final AtomicLong runs = new AtomicLong();

  static {
    String s = System.getProperty("scalac.uniques", "weak");
    switch (s) {
      case "weak": MODE = WEAK; break;
      case "direct": MODE = DIRECT; break;
      case "off": MODE = OFF; break;
      default: throw new IllegalArgumentException("scalac.uniques=" + s);
    }
    if (STATS) {
      Runtime.getRuntime().addShutdownHook(new Thread(() -> {
        long h = hits.get(), m = misses.get();
        System.err.printf("[scalac.uniques=%s] runs=%d hits=%d misses=%d hitRate=%.4f%n",
            s, runs.get(), h, m, (h + m) == 0 ? 0.0 : (double) h / (h + m));
      }));
    }
  }

  private UniquesMode() {}
}
