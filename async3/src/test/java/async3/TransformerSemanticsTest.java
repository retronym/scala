package async3;

import async3.samples.RejectedSamples;
import async3.samples.Samples;
import async3.transform.AsyncTransformer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.concurrent.CompletableFuture;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Phase 1/2: the ASM-transformed {@code <name>$async} variants must agree with the blocking tier,
 * on both the already-completed fast path ("done") and the genuine suspension path ("later").
 * Loading the transformed classes through a fresh class loader runs the JVM verifier over every
 * generated method — the strongest available check that the rewrite produces valid frames.
 */
class TransformerSemanticsTest {

    static Class<?> host;

    @BeforeAll
    static void transform() {
        host = transformAndLoad(Samples.class);
    }

    @Test
    void addOne() throws Throwable {
        assertEquals(21, invokeAsync(host, "addOne", done(20)));
        assertEquals(21, invokeAsync(host, "addOne", later(20)));
    }

    @Test
    void addOneFailure() {
        IllegalStateException boom = new IllegalStateException("boom");
        assertSame(boom, assertThrows(IllegalStateException.class,
                () -> invokeAsync(host, "addOne", TestSupport.<Integer>doneFailed(boom))));
        assertSame(boom, assertThrows(IllegalStateException.class,
                () -> invokeAsync(host, "addOne", TestSupport.<Integer>laterFailed(boom))));
    }

    @Test
    void sumTwice() throws Throwable {
        assertEquals("s:10", invokeAsync(host, "sumTwice", done(5), done("s")));
        assertEquals("s:10", invokeAsync(host, "sumTwice", later(5), later("s")));
        assertEquals("s:10", invokeAsync(host, "sumTwice", done(5), later("s")));
        assertEquals("s:10", invokeAsync(host, "sumTwice", later(5), done("s")));
    }

    /** The case the tree-level ANF transform exists to forbid: await with values on the stack. */
    @Test
    void deepStack() throws Throwable {
        // 1 + 2*5 + 7*3 = 32
        assertEquals(32, invokeAsync(host, "deepStack", done(5), done(7)));
        assertEquals(32, invokeAsync(host, "deepStack", later(5), later(7)));
    }

    @Test
    void loopSum() throws Throwable {
        CompletableFuture<?>[] fs = {done(1), later(2), done(3), later(4), done(5)};
        assertEquals(15, invokeAsync(host, "loopSum", (Object) fs));
    }

    /** A failed future awaited inside try must reach the user's catch handler. */
    @Test
    void tryCatch() throws Throwable {
        assertEquals("ok:5", invokeAsync(host, "tryCatch", done(5)));
        assertEquals("ok:5", invokeAsync(host, "tryCatch", later(5)));
        IllegalStateException boom = new IllegalStateException("boom");
        assertEquals("caught:boom", invokeAsync(host, "tryCatch", TestSupport.<Integer>doneFailed(boom)));
        assertEquals("caught:boom", invokeAsync(host, "tryCatch", TestSupport.<Integer>laterFailed(boom)));
    }

    @Test
    void mixedPrims() throws Throwable {
        // l = 12; d = 2.5; d2 = 2.5 + 0.5 = 3.0; 2.5 * 12 + 3.0 = 33.0
        assertEquals(33.0d, invokeAsync(host, "mixedPrims", done(2.5d), 4L));
        assertEquals(33.0d, invokeAsync(host, "mixedPrims", later(2.5d), 4L));
    }

    @Test
    void nullLocal() throws Throwable {
        assertEquals("pos:6", invokeAsync(host, "nullLocal", done(3)));
        assertEquals("pos:6", invokeAsync(host, "nullLocal", later(3)));
    }

    @Test
    void initializedAcrossAwait() throws Throwable {
        assertEquals("v=7", invokeAsync(host, "initializedAcrossAwait", done(7)));
        assertEquals("v=7", invokeAsync(host, "initializedAcrossAwait", later(7)));
    }

    /** The original methods are untouched and still run as the blocking tier. */
    @Test
    void blockingTierStillWorks() throws Throwable {
        assertEquals("s:10", invoke(host, "sumTwice", done(5), later("s")));
        assertEquals(32, invoke(host, "deepStack", later(5), done(7)));
    }

    /** Phase 3 seed: frame-slot metadata with source variable names is emitted per state. */
    @Test
    void debugMetadata() {
        AsyncTransformer.Result result = AsyncTransformer.transform(classBytes(Samples.class));
        String meta = result.debugMetadata.get("async3.samples.Samples.sumTwice");
        assertNotNull(meta);
        assertTrue(meta.contains("state 1"), meta);
        assertTrue(meta.contains("state 2"), meta);
        // by state 2 the int local `x` is live and spilled to a prims slot, under its source name
        assertTrue(meta.contains("x -> prims["), meta);
        // System.out.println for exploration:
        result.debugMetadata.forEach((k, v) -> System.out.println("== " + k + "\n" + v));
    }

    @Test
    void rejectsAwaitUnderMonitor() {
        UnsupportedOperationException e = assertThrows(UnsupportedOperationException.class,
                () -> AsyncTransformer.transform(classBytes(RejectedSamples.Monitor.class)));
        assertTrue(e.getMessage().contains("monitor"), e.getMessage());
    }
}
