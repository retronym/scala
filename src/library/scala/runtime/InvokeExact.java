package scala.runtime;

import java.lang.invoke.*;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class InvokeExact {
    private static MethodHandles.Lookup TRUSTED = getTrustedLookup();

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName, MethodType invokedType,
                                     Class<?> fromSite) throws Throwable {
        Method method = fromSite.getMethod(invokedName, invokedType.dropParameterTypes(0, 1).parameterArray());
        return new ConstantCallSite(TRUSTED.unreflectSpecial(method, fromSite));
    }

    private static MethodHandles.Lookup getTrustedLookup() {
        try {
            Field field = MethodHandles.Lookup.class.getDeclaredField("IMPL_LOOKUP");
            field.setAccessible(true);
            return (MethodHandles.Lookup) field.get(null);
        } catch (ReflectiveOperationException e) {
            return null;
        }
    }
}
