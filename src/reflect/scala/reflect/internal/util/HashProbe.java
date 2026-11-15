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

/** Probe flags for testing whether type hash codes are reproducible across JVMs. Static finals, so the JIT folds them. */
public final class HashProbe {
  private HashProbe() {}

  /** `-Dscalac.symbolHashById=true`: `Symbol.hashCode` is the symbol's `id`, not its identity hash. */
  public static final boolean SYMBOL_HASH_BY_ID = Boolean.getBoolean("scalac.symbolHashById");

  /** `-Dscalac.stableHash=true`: `TypeVar.hashCode` and `Scope.hashCode` are per-run ids, not identity hashes. */
  public static final boolean STABLE_HASH = Boolean.getBoolean("scalac.stableHash");

  /** `-Dscalac.treeHashById=true`: `Tree.hashCode` is the tree's `id`. */
  public static final boolean TREE_HASH_BY_ID = Boolean.getBoolean("scalac.treeHashById");

  /** `-Dscalac.hashdump=<dir>`: dump typed trees' type hashes and the uniques table after each run. */
  public static final String HASH_DUMP_DIR = System.getProperty("scalac.hashdump");
}
