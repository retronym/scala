package scala.runtime;


import java.lang.invoke.*;
import java.util.Arrays;

public final class StructuralCall {
    private StructuralCall() {
    }

    private static final MethodType INVOKE_METHOD_TYPE = MethodType.methodType(Object.class, new Class<?>[]{Object.class, Object[].class});

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType) throws Throwable {
        Class<?>[] parameterArray = invokedType.parameterArray();
        Class<?>[] paramsWithoutThis = Arrays.copyOfRange(parameterArray, 1, parameterArray.length);
        MethodCacheRef methodCacheRef = new MethodCacheRef(invokedName, paramsWithoutThis);
        MethodHandle invoke = lookup.findVirtual(MethodCacheRef.class, "invoke", INVOKE_METHOD_TYPE);
        MethodHandle bound = invoke.bindTo(methodCacheRef);
        MethodHandle collect = bound.asCollector(Object[].class, paramsWithoutThis.length);
        MethodHandle exact = collect.asType(invokedType);
        return new ConstantCallSite(exact);
    }
}
