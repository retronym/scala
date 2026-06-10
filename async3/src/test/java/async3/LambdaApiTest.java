package async3;

import async3.runtime.Async;
import org.junit.jupiter.api.Test;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * The lambda front end: {@code Async.async(() -> ... Async.await(f) ...)}, triggered by lambda
 * cracking and defined as a hidden nestmate class. No pre-transformed classes, no reflection
 * in user code.
 */
class LambdaApiTest {

    @Test
    void sequentialAwaits() {
        CompletableFuture<Integer> fa = later(5);
        CompletableFuture<String> fb = later("s");
        CompletableFuture<String> r = Async.async(() -> {
            int x = Async.await(fa);
            String s = Async.await(fb);
            return s + ":" + (x * 2);
        });
        assertEquals("s:10", r.join());
    }

    @Test
    void capturesLocals() {
        int base = 100;
        String tag = "t";
        CompletableFuture<Integer> f = later(7);
        assertEquals("t:107", Async.async(() -> tag + ":" + (base + Async.await(f))).join());
    }

    static class Counter {
        private int count; // private state touched from the transformed body => nestmate proof

        CompletableFuture<Integer> bumpTwice(CompletableFuture<Integer> f) {
            return Async.async(() -> {
                count += Async.await(f);
                count += Async.await(f);
                return count;
            });
        }
    }

    @Test
    void capturesThisAndPrivateState() {
        assertEquals(6, new Counter().bumpTwice(later(3)).join());
    }

    @Test
    void failureFailsTheFuture() {
        IllegalStateException boom = new IllegalStateException("boom");
        CompletableFuture<Integer> f = laterFailed(boom);
        CompletableFuture<Integer> r = Async.async(() -> Async.await(f) + 1);
        CompletionException e = assertThrows(CompletionException.class, r::join);
        assertSame(boom, e.getCause());
    }

    @Test
    void tryCatchInsideLambda() {
        CompletableFuture<Integer> f = laterFailed(new IllegalStateException("boom"));
        String r = Async.<String>async(() -> {
            try {
                return "ok:" + Async.await(f);
            } catch (IllegalStateException e) {
                return "caught:" + e.getMessage();
            }
        }).join();
        assertEquals("caught:boom", r);
    }

    @Test
    void loopInsideLambda() {
        CompletableFuture<Integer>[] fs = futures(1, 2, 3, 4, 5);
        CompletableFuture<Integer> r = Async.async(() -> {
            int sum = 0;
            for (CompletableFuture<Integer> f : fs) sum += Async.await(f);
            return sum;
        });
        assertEquals(15, r.join());
    }

    @Test
    void noAwait() {
        assertEquals(42, Async.async(() -> 42).join());
    }

    /** Same lambda (= same impl method) invoked repeatedly hits the constructor cache. */
    @Test
    void repeatedInvocationsOfTheSameLambda() {
        for (int i = 0; i < 50; i++) {
            CompletableFuture<Integer> f = i % 2 == 0 ? done(i) : later(i);
            int expected = i + 1;
            assertEquals(expected, Async.<Integer>async(() -> Async.await(f) + 1).join());
        }
    }

    /** The debuggable mode (ordinary findable class, no nest) handles non-private bodies. */
    @Test
    void debuggableMode() {
        System.setProperty("async3.lambda.debuggable", "true");
        try {
            CompletableFuture<Integer> f = later(20);
            assertEquals(21, Async.<Integer>async(() -> Async.await(f) + 1).join());
        } finally {
            System.clearProperty("async3.lambda.debuggable");
        }
    }

    @SuppressWarnings("unchecked")
    private static CompletableFuture<Integer>[] futures(int... values) {
        CompletableFuture<Integer>[] fs = new CompletableFuture[values.length];
        for (int i = 0; i < values.length; i++)
            fs[i] = i % 2 == 0 ? done(values[i]) : later(values[i]);
        return fs;
    }
}
