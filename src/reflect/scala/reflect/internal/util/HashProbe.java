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

  /** `-Dscalac.hashSeed=<int>`: seed the id-based hash codes below, so that a campaign can sample hash layouts
   *  deliberately (one seed per fork, the same seeds for every variant). Setting it turns on all three id modes. */
  public static final boolean HAS_SEED = System.getProperty("scalac.hashSeed") != null;
  public static final int SEED = HAS_SEED ? Integer.parseInt(System.getProperty("scalac.hashSeed")) : 0;

  /** `-Dscalac.symbolHashById=true`: `Symbol.hashCode` is the symbol's `id`, not its identity hash. */
  public static final boolean SYMBOL_HASH_BY_ID = HAS_SEED || Boolean.getBoolean("scalac.symbolHashById");

  /** `-Dscalac.stableHash=true`: `TypeVar.hashCode` and `Scope.hashCode` are per-run ids, not identity hashes. */
  public static final boolean STABLE_HASH = HAS_SEED || Boolean.getBoolean("scalac.stableHash");

  /** `-Dscalac.treeHashById=true`: `Tree.hashCode` is the tree's `id`. */
  public static final boolean TREE_HASH_BY_ID = HAS_SEED || Boolean.getBoolean("scalac.treeHashById");

  /** The hash code for a per-run `id`: `id` itself without a seed, else `id` scrambled with the seed (MurmurHash3's
   *  finalizer, a bijection, so distinct ids keep distinct hashes). `kind` separates the id spaces of symbols, trees,
   *  type variables and scopes. */
  public static int idHash(int id, int kind) {
    if (!HAS_SEED) return id;
    int h = id ^ (SEED * 0x9E3779B9) ^ (kind * 0x85EBCA6B);
    h ^= h >>> 16;
    h *= 0x85EBCA6B;
    h ^= h >>> 13;
    h *= 0xC2B2AE35;
    h ^= h >>> 16;
    return h;
  }

  /** `-Dscalac.hashdump=<dir>`: dump typed trees' type hashes and the uniques table after each run. */
  public static final String HASH_DUMP_DIR = System.getProperty("scalac.hashdump");
}
