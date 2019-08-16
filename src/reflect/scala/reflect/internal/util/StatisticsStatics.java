/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal.util;

import scala.reflect.internal.util.AlmostFinalValue;
import java.lang.invoke.MethodHandle;

public final class StatisticsStatics {
  public static final boolean areSomeColdStatsEnabled = false;
  public static final boolean areSomeHotStatsEnabled = false;

  public static void enableColdStats() throws Throwable {
  }

  public static void enableHotStats() throws Throwable {
  }
}
