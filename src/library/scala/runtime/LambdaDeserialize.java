package scala.runtime;


import java.lang.invoke.*;
import java.util.Arrays;
import java.util.HashMap;

public final class LambdaDeserialize {
    private LambdaDeserialize() {
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType) throws Throwable {
        HashMap<String, MethodHandle> cache = new HashMap<>();
        LambdaDeserializer$ l = LambdaDeserializer$.MODULE$;
        MethodType type = MethodType.fromMethodDescriptorString("(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/util/Map;Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", lookup.getClass().getClassLoader());
        MethodHandle deserializeLambda = lookup.findVirtual(LambdaDeserializer$.class, "deserializeLambda", type);
        MethodHandle bound = deserializeLambda.bindTo(l).bindTo(lookup).bindTo(cache);
        MethodHandle exact = bound.asType(invokedType);
        return new ConstantCallSite(exact);
    }
}
