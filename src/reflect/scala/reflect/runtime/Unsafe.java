/* NSC  new Scala compiler
 * Copyright 20052017 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.runtime;

import java.lang.reflect.Field;

final class Unsafe {
    public final static sun.misc.Unsafe U;

    static {
        U = lookupUnsafe();
    }

    private static sun.misc.Unsafe lookupUnsafe() {
        try {
            sun.misc.Unsafe found = null;
            for (Field field : sun.misc.Unsafe.class.getDeclaredFields()) {
                if (field.getType() == sun.misc.Unsafe.class) {
                    field.setAccessible(true);
                    found = (sun.misc.Unsafe) field.get(null);
                    break;
                }
            }
            if (found == null) throw new IllegalStateException("Can't find instance of sun.misc.Unsafe");
            else return found;
        } catch (Throwable t) {
            throw new ExceptionInInitializerError(t);
        }
    }

}

// Scala version:
// classOf[sun.misc.Unsafe].getDeclaredFields.filter(_.getType == classOf[sun.misc.Unsafe]).headOption.map { field =>
//   field.setAccessible(true); field.get(null).asInstanceOf[sun.misc.Unsafe]
// } getOrElse (throw new IllegalStateException("Can't find instance of sun.misc.Unsafe"))