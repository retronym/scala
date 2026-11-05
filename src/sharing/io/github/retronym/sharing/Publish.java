package io.github.retronym.sharing;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

/** Release/acquire access to the slots of an array (an open-addressing hash table read by several threads): an entry
 *  stored with release semantics is seen complete by a reader that loads its slot with acquire semantics. On x86 an
 *  acquire load is a plain load; on Apple silicon it is `ldar`.
 *
 *  A field that publishes others is a `Published` instead, with its handle declared beside its class.
 *
 *  A Java class, so that the handle is a `static final` constant the JIT folds.
 *
 *  The one class of the library that names `VarHandle` (JDK 9) in the Scala 2.13 build: the field accesses that
 *  `Published` and `Once` expand to there are the static methods below, which take the handle as an `Object`, so that
 *  the rest of that build needs only the JDK 8 API. Each is a signature-polymorphic call with a fixed descriptor, small
 *  enough to inline at the site, where a handle loaded from a constant folds.
 */
public final class Publish {
  private Publish() {}

  private static final VarHandle SLOT = MethodHandles.arrayElementVarHandle(Object[].class);

  public static Object slotAcquire(Object[] array, int i) {
    return SLOT.getAcquire(array, i);
  }

  public static void setSlotRelease(Object[] array, int i, Object value) {
    SLOT.setRelease(array, i, value);
  }

  // ---- fields, for the Scala 2.13 macros

  /** The `VarHandle` of field `name` of class `owner`, of erased type `type`. */
  public static Object fieldHandle(Class<?> owner, String name, Class<?> type) {
    try {
      return MethodHandles.privateLookupIn(owner, MethodHandles.lookup()).findVarHandle(owner, name, type);
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException("no field " + owner.getName() + "." + name + " of type " + type.getName(), e);
    }
  }

  public static int getAcquireInt(Object vh, Object o) { return (int) ((VarHandle) vh).getAcquire(o); }
  public static void setReleaseInt(Object vh, Object o, int v) { ((VarHandle) vh).setRelease(o, v); }
  public static long getAcquireLong(Object vh, Object o) { return (long) ((VarHandle) vh).getAcquire(o); }
  public static void setReleaseLong(Object vh, Object o, long v) { ((VarHandle) vh).setRelease(o, v); }
  public static boolean getAcquireBoolean(Object vh, Object o) { return (boolean) ((VarHandle) vh).getAcquire(o); }
  public static void setReleaseBoolean(Object vh, Object o, boolean v) { ((VarHandle) vh).setRelease(o, v); }
  public static Object getAcquireRef(Object vh, Object o) { return (Object) ((VarHandle) vh).getAcquire(o); }
  public static void setReleaseRef(Object vh, Object o, Object v) { ((VarHandle) vh).setRelease(o, v); }
}
