package async3.runtime;

import async3.transform.AsyncTransformer;

import java.io.InputStream;
import java.io.Serializable;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Lambda-based front end: the transform is triggered at runtime by <em>cracking</em> the lambda.
 *
 * <pre>{@code
 * CompletableFuture<String> r = Async.async(() -> {
 *     int x = Async.await(fa);
 *     String s = Async.await(fb);
 *     return s + ":" + (x * 2);
 * });
 * }</pre>
 *
 * <p>How it works: {@link AsyncBody} extends {@link Serializable}, so javac emits a
 * {@code writeReplace} on the lambda proxy. Invoking it yields a {@link SerializedLambda}, which
 * names the synthetic {@code lambda$...} impl method (where the body's bytecode actually lives,
 * in the capturing class) and carries the captured arguments. The impl method is run through
 * {@link AsyncTransformer#transformMethod} and the state machine is defined as a
 * <em>hidden class with the NESTMATE option</em>, so the transformed body keeps access to
 * private members of the capturing class — exactly what lambda bodies are allowed to touch.
 * Captured locals (including a captured {@code this}) are the impl method's leading
 * parameters, which the state machine constructor spills like any other entry locals.
 * Compilation happens once per impl method; subsequent calls reuse the cached constructor.
 *
 * <p>This is a faithful stand-in for the eventual compiler integration: an {@code async { ... }}
 * compiled to {@code invokedynamic} hands its bootstrap the caller's {@link MethodHandles.Lookup}
 * with no cracking or reflection needed — everything downstream of the lookup is identical.
 *
 * <p>Modes:
 * <ul>
 *   <li>default: hidden class + NESTMATE (no class name leaks, full private access to the
 *       capturing class). IDE line breakpoints inside the lambda body will NOT bind: IntelliJ
 *       resolves such lines only against classes named like the enclosing source class;</li>
 *   <li>{@code -Dasync3.lambda.debuggable=true}: defines the state machine as an ordinary class
 *       <em>named exactly like the capturing class</em>, in a throwaway loader (JDI matches
 *       classes by name across loaders, so the IDE finds it and line breakpoints in the lambda
 *       body bind and fire). The trade-off: from inside the shadow class, the host's name
 *       resolves to the shadow itself, so lambdas that reference the host class — captured
 *       {@code this}, same-class helper calls, nested lambdas — are rejected with an
 *       explanatory error in this mode;</li>
 *   <li>{@code -Dasync3.lambda.dump=<dir>}: also writes the generated class for javap.</li>
 * </ul>
 *
 * <p>An eventual compiler/agent integration sidesteps the trade-off entirely by emitting the
 * resumable body as a sibling method <em>of the capturing class itself</em> (the
 * `externalFsmMethod` shape of the annotation-driven frontend) — then breakpoints, nest access,
 * and stepping all work with no shadowing.
 */
public final class Async {
    private Async() {}

    /** Serializable so the lambda can be cracked via {@code writeReplace}. */
    @FunctionalInterface
    public interface AsyncBody<T> extends Serializable {
        T call();
    }

    /** The await marker; identical semantics to {@link AsyncRT#await} (tier 0 blocks). */
    public static <T> T await(CompletableFuture<T> future) {
        return AsyncRT.await(future);
    }

    /**
     * Runs {@code body} as a state machine. Resolves private access via
     * {@code privateLookupIn(capturingClass)}, which works whenever the capturing class is
     * accessible to this class's module (always true on the plain classpath).
     */
    public static <T> CompletableFuture<T> async(AsyncBody<T> body) {
        return asyncImpl(null, body);
    }

    /** Variant for named-module callers: pass {@code MethodHandles.lookup()} from the call site. */
    public static <T> CompletableFuture<T> async(MethodHandles.Lookup lookup, AsyncBody<T> body) {
        return asyncImpl(lookup, body);
    }

    // ------------------------------------------------------------------ lift: method references

    // Serializable SAM types so a method reference can be cracked. Arity disambiguates overloads.
    @FunctionalInterface public interface Fn0<R> extends Serializable { R apply(); }
    @FunctionalInterface public interface Fn1<A, R> extends Serializable { R apply(A a); }
    @FunctionalInterface public interface Fn2<A, B, R> extends Serializable { R apply(A a, B b); }
    @FunctionalInterface public interface Fn3<A, B, C, R> extends Serializable { R apply(A a, B b, C c); }

    @FunctionalInterface public interface Lifted0<R> { CompletableFuture<R> apply(); }
    @FunctionalInterface public interface Lifted1<A, R> { CompletableFuture<R> apply(A a); }
    @FunctionalInterface public interface Lifted2<A, B, R> { CompletableFuture<R> apply(A a, B b); }
    @FunctionalInterface public interface Lifted3<A, B, C, R> { CompletableFuture<R> apply(A a, B b, C c); }

    /**
     * Lifts the referenced method into its suspending variant: the runtime analogue of the
     * annotation-driven frontend's direct/{@code $queued} method pair. The target — possibly in
     * another class — is any method whose bytecode calls the await markers; called directly it
     * blocks (tier 0), called through the lifted handle it suspends. Works with static,
     * unbound-instance ({@code C::m} — the receiver becomes the first parameter), and
     * bound-instance ({@code obj::m} — the receiver is a captured argument) references, and with
     * lambdas. A target with no awaits lifts to a trivially-completed future. Compiled once per
     * target method, cached.
     */
    public static <R> Lifted0<R> lift(Fn0<R> ref) {
        Lifted l = liftImpl(ref);
        return () -> cast(l.invoke());
    }

    public static <A, R> Lifted1<A, R> lift(Fn1<A, R> ref) {
        Lifted l = liftImpl(ref);
        return a -> cast(l.invoke(a));
    }

    public static <A, B, R> Lifted2<A, B, R> lift(Fn2<A, B, R> ref) {
        Lifted l = liftImpl(ref);
        return (a, b) -> cast(l.invoke(a, b));
    }

    public static <A, B, C, R> Lifted3<A, B, C, R> lift(Fn3<A, B, C, R> ref) {
        Lifted l = liftImpl(ref);
        return (a, b, c) -> cast(l.invoke(a, b, c));
    }

    private static Lifted liftImpl(Serializable ref) {
        SerializedLambda sl = crack(ref);
        if (sl.getImplMethodKind() == java.lang.invoke.MethodHandleInfo.REF_newInvokeSpecial)
            throw new UnsupportedOperationException(
                    "constructor references cannot be lifted (await is not supported in constructors)");
        MethodHandle ctor = constructorFor(null, ref, sl);
        return new Lifted(ctor, capturedArgs(sl));
    }

    /**
     * Invokes the cached state machine constructor with captured arguments (bound receiver, for
     * {@code obj::m}) followed by per-call arguments — together they form the target method's
     * entry locals in order, for every reference shape.
     */
    private record Lifted(MethodHandle ctor, Object[] captured) {
        CompletableFuture<Object> invoke(Object... args) {
            Object[] all = new Object[captured.length + args.length];
            System.arraycopy(captured, 0, all, 0, captured.length);
            System.arraycopy(args, 0, all, captured.length, args.length);
            try {
                return ((FutureStateMachine) ctor.invokeWithArguments(all)).start();
            } catch (Throwable t) {
                throw AsyncRT.sneakyThrow(t);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static <R> CompletableFuture<R> cast(CompletableFuture<Object> f) {
        return (CompletableFuture<R>) (CompletableFuture<?>) f;
    }

    private static final ConcurrentHashMap<String, MethodHandle> CONSTRUCTOR_CACHE = new ConcurrentHashMap<>();

    private static <T> CompletableFuture<T> asyncImpl(MethodHandles.Lookup lookup, AsyncBody<T> body) {
        SerializedLambda sl = crack(body);
        MethodHandle ctor = constructorFor(lookup, body, sl);
        return cast(new Lifted(ctor, capturedArgs(sl)).invoke());
    }

    private static MethodHandle constructorFor(MethodHandles.Lookup lookup, Serializable ref, SerializedLambda sl) {
        String key = (debuggable() ? "shadow:" : "hidden:")
                + sl.getImplClass() + "." + sl.getImplMethodName() + sl.getImplMethodSignature();
        return CONSTRUCTOR_CACHE.computeIfAbsent(key, k -> compile(lookup, ref.getClass().getClassLoader(), sl));
    }

    private static Object[] capturedArgs(SerializedLambda sl) {
        Object[] captured = new Object[sl.getCapturedArgCount()];
        for (int i = 0; i < captured.length; i++) captured[i] = sl.getCapturedArg(i);
        return captured;
    }

    private static SerializedLambda crack(Serializable lambdaOrRef) {
        try {
            Method writeReplace = lambdaOrRef.getClass().getDeclaredMethod("writeReplace");
            writeReplace.setAccessible(true);
            return (SerializedLambda) writeReplace.invoke(lambdaOrRef);
        } catch (ReflectiveOperationException e) {
            throw new IllegalArgumentException(
                    "expected a lambda literal or method reference, not a class implementing the interface", e);
        }
    }

    private static MethodHandle compile(MethodHandles.Lookup callerLookup, ClassLoader loader, SerializedLambda sl) {
        try {
            Class<?> capturingClass = Class.forName(sl.getImplClass().replace('/', '.'), false, loader);

            byte[] hostBytes;
            try (InputStream in = loader.getResourceAsStream(sl.getImplClass() + ".class")) {
                if (in == null)
                    throw new IllegalStateException("cannot read bytecode of " + sl.getImplClass());
                hostBytes = in.readAllBytes();
            } catch (java.io.IOException e) {
                throw new IllegalStateException(e);
            }

            boolean shadow = debuggable();
            AsyncTransformer.SingleMethod sm = AsyncTransformer.transformMethod(
                    hostBytes, sl.getImplMethodName(), sl.getImplMethodSignature(), shadow);

            String dumpDir = System.getProperty("async3.lambda.dump");
            if (dumpDir != null) {
                // shadow classes share the host's name; suffix the dump file to avoid clobbering
                Path p = Path.of(dumpDir).resolve(
                        sm.name.replace('.', '/') + (shadow ? "$shadow$" + sl.getImplMethodName() : "") + ".class");
                Files.createDirectories(p.getParent());
                Files.write(p, sm.bytes);
            }

            if (shadow) {
                Class<?> smClass = new OneShotLoader(loader).define(sm.name, sm.bytes);
                return MethodHandles.lookup().unreflectConstructor(smClass.getDeclaredConstructors()[0]);
            }

            MethodHandles.Lookup lookup = callerLookup != null
                    ? callerLookup
                    : MethodHandles.privateLookupIn(capturingClass, MethodHandles.lookup());
            MethodHandles.Lookup smLookup =
                    lookup.defineHiddenClass(sm.bytes, false, MethodHandles.Lookup.ClassOption.NESTMATE);
            MethodType ctorType = MethodType.fromMethodDescriptorString(sm.constructorDescriptor, loader);
            return smLookup.findConstructor(smLookup.lookupClass(), ctorType);
        } catch (ReflectiveOperationException | java.io.IOException e) {
            throw new IllegalStateException("failed to compile async lambda " + sl.getImplMethodName(), e);
        }
    }

    private static boolean debuggable() {
        return Boolean.getBoolean("async3.lambda.debuggable");
    }

    private static final class OneShotLoader extends ClassLoader {
        OneShotLoader(ClassLoader parent) { super(parent); }
        Class<?> define(String name, byte[] bytes) { return defineClass(name, bytes, 0, bytes.length); }
    }
}
