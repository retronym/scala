package async3;

import async3.transform.AsyncTransformer;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
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

    static ClassLoader loaderFor(AsyncTransformer.Result result) {
        return new async3.transform.InMemoryClassLoader(result, TestSupport.class.getClassLoader());
    }

    static Class<?> transformAndLoad(Class<?> source) {
        AsyncTransformer.Result result = AsyncTransformer.transform(classBytes(source));
        try {
            return Class.forName(result.hostName, true, loaderFor(result));
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
        return invokeOn(host, null, name, args);
    }

    /** Instance variant of {@link #invokeAsync}. */
    static Object invokeAsyncOn(Object target, String name, Object... args) throws Throwable {
        CompletableFuture<?> f = (CompletableFuture<?>) invokeOn(target.getClass(), target, name + "$async", args);
        try {
            return f.get(5, TimeUnit.SECONDS);
        } catch (ExecutionException e) {
            throw e.getCause();
        }
    }

    static Object invokeOn(Class<?> host, Object target, String name, Object... args) throws Throwable {
        Method m = null;
        for (Method cand : host.getDeclaredMethods())
            if (cand.getName().equals(name)) { m = cand; break; }
        if (m == null) throw new NoSuchMethodException(host.getName() + "." + name);
        try {
            return m.invoke(target, args);
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
    }

    static Object newInstance(Class<?> host, Object... ctorArgs) throws Throwable {
        for (Constructor<?> c : host.getDeclaredConstructors())
            if (c.getParameterCount() == ctorArgs.length) {
                try {
                    return c.newInstance(ctorArgs);
                } catch (InvocationTargetException e) {
                    throw e.getCause();
                }
            }
        throw new NoSuchMethodException(host.getName() + ".<init>/" + ctorArgs.length);
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
