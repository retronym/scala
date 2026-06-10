package async3;

import async3.transform.AsyncTransformer;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

final class TestSupport {
    private TestSupport() {}

    static byte[] classBytes(Class<?> c) {
        String resource = c.getName().replace('.', '/') + ".class";
        try (InputStream in = c.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) throw new IllegalStateException("not found: " + resource);
            return in.readAllBytes();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /** Loads the patched host class plus generated state machines; child-first for those names. */
    static final class TransformedLoader extends ClassLoader {
        private final Map<String, byte[]> classes;

        TransformedLoader(AsyncTransformer.Result result) {
            super(TransformedLoader.class.getClassLoader());
            this.classes = result.allClasses();
        }

        @Override
        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            synchronized (getClassLoadingLock(name)) {
                Class<?> c = findLoadedClass(name);
                if (c == null) {
                    byte[] b = classes.get(name);
                    // The JVM verifier runs on these defineClass calls — it is the prototype's
                    // bytecode-correctness check.
                    c = b != null ? defineClass(name, b, 0, b.length) : super.loadClass(name, false);
                }
                if (resolve) resolveClass(c);
                return c;
            }
        }
    }

    static Class<?> transformAndLoad(Class<?> source) {
        AsyncTransformer.Result result = AsyncTransformer.transform(classBytes(source));
        try {
            return Class.forName(result.hostName, true, new TransformedLoader(result));
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    /** Invokes {@code name$async}, awaits the resulting future, unwrapping failures. */
    static Object invokeAsync(Class<?> host, String name, Object... args) throws Throwable {
        CompletableFuture<?> f = (CompletableFuture<?>) invoke(host, name + "$async", args);
        try {
            return f.get(5, TimeUnit.SECONDS);
        } catch (ExecutionException e) {
            throw e.getCause();
        }
    }

    /** Invokes the untouched synchronous method (the blocking tier). */
    static Object invoke(Class<?> host, String name, Object... args) throws Throwable {
        Method m = null;
        for (Method cand : host.getDeclaredMethods())
            if (cand.getName().equals(name)) { m = cand; break; }
        if (m == null) throw new NoSuchMethodException(host.getName() + "." + name);
        try {
            return m.invoke(null, args);
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
    }

    // ---- future factories: "done" exercises the fast path, "later" the real suspension path

    static <T> CompletableFuture<T> done(T value) {
        return CompletableFuture.completedFuture(value);
    }

    static <T> CompletableFuture<T> doneFailed(Throwable t) {
        CompletableFuture<T> f = new CompletableFuture<>();
        f.completeExceptionally(t);
        return f;
    }

    static <T> CompletableFuture<T> later(T value) {
        CompletableFuture<T> f = new CompletableFuture<>();
        CompletableFuture.delayedExecutor(5, TimeUnit.MILLISECONDS).execute(() -> f.complete(value));
        return f;
    }

    static <T> CompletableFuture<T> laterFailed(Throwable t) {
        CompletableFuture<T> f = new CompletableFuture<>();
        CompletableFuture.delayedExecutor(5, TimeUnit.MILLISECONDS).execute(() -> f.completeExceptionally(t));
        return f;
    }
}
