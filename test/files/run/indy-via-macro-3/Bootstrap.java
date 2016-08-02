package test;

import java.lang.invoke.*;

public final class Bootstrap {
    private Bootstrap() {
    }
    private static final Object U = getUnsafeOrNull();
    private static final MethodType OBJECT_FIELD_OFFSET_DESC =
            MethodType.methodType(Long.TYPE, java.lang.reflect.Field.class);
    private static final MethodType UNSAFE_CAS_INT_DESC =
            MethodType.methodType(java.lang.Boolean.TYPE, Object.class, Long.TYPE, Integer.TYPE, Integer.TYPE);

    /** Pre-compile a regex */
    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType,
                                     Class<?> cls, String fieldName) throws Throwable {
        if (U != null) {
            MethodHandle objectFieldOffset = MethodHandles.lookup().findVirtual(U.getClass(), "objectFieldOffset", OBJECT_FIELD_OFFSET_DESC);
            long offset = (long) objectFieldOffset.invokeExact(cls.getDeclaredField(fieldName));
            MethodHandle cas = MethodHandles.lookup().findVirtual(U.getClass(), "compareAndSwapInt", UNSAFE_CAS_INT_DESC);
            // [perform access checks]
            return new ConstantCallSite(MethodHandles.insertArguments(cas.bindTo(U), 1, offset));
        } else {
            throw new UnsupportedOperationException("TODO: Detect and use VarHandle");
        }
    }

    @SuppressWarnings("restriction")
    private static Object getUnsafeOrNull() {
        try {

            java.lang.reflect.Field singleoneInstanceField = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe");
            singleoneInstanceField.setAccessible(true);
            return singleoneInstanceField.get(null);

        } catch (Throwable e) {
            return null;
        }
    }
}
