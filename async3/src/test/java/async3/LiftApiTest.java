package async3;

import async3.runtime.Async;
import async3.samples.InstanceSamples;
import async3.samples.Samples;
import org.junit.jupiter.api.Test;

import java.util.concurrent.CompletableFuture;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@code Async.lift(C::m)}: lifting an existing method — possibly from another class — into its
 * suspending variant at runtime. The runtime analogue of the annotation-driven frontend's
 * direct/{@code $queued} method pair: the author writes one method calling the await markers;
 * direct calls block (tier 0), the lifted handle suspends.
 */
class LiftApiTest {

    static class Helpers {
        static String greet(CompletableFuture<String> f, int n) {
            return Async.await(f) + n;
        }

        private static int privAdd(CompletableFuture<Integer> f, int n) {
            return Async.await(f) + n;
        }

        static int noAwait(int x) {
            return x * 2;
        }
    }

    /** Cross-class static method reference; Samples.sumTwice was written long before lift existed. */
    @Test
    void crossClassStaticRef() {
        Async.Lifted2<CompletableFuture<Integer>, CompletableFuture<String>, String> sumTwice =
                Async.lift(Samples::sumTwice);
        assertEquals("s:10", sumTwice.apply(later(5), later("s")).join());
        assertEquals("s:10", sumTwice.apply(done(5), done("s")).join());
    }

    @Test
    void staticRef() {
        assertEquals("hi7", Async.lift(Helpers::greet).apply(later("hi"), 7).join());
    }

    /** Private target: the hidden state machine joins the target's nest. */
    @Test
    void privateMethodRef() {
        assertEquals(10, Async.lift(Helpers::privAdd).apply(later(3), 7).join());
    }

    /** Unbound instance reference: the receiver becomes the first parameter. */
    @Test
    void unboundInstanceRef() {
        Async.Lifted3<InstanceSamples, CompletableFuture<Integer>, CompletableFuture<Integer>, String> compute =
                Async.lift(InstanceSamples::compute);
        assertEquals("L*:17", compute.apply(new InstanceSamples(10, "L"), later(3), later(4)).join());
    }

    /** Bound instance reference: the receiver is a captured argument. */
    @Test
    void boundInstanceRef() {
        InstanceSamples inst = new InstanceSamples(10, "L");
        Async.Lifted2<CompletableFuture<Integer>, CompletableFuture<Integer>, String> compute =
                Async.lift(inst::compute);
        assertEquals("L*:17", compute.apply(later(3), done(4)).join());
        assertEquals("L**:17", compute.apply(done(3), later(4)).join()); // same bound receiver, mutated
    }

    /** A target with no awaits lifts to a trivially-completed future. */
    @Test
    void noAwaitTarget() {
        assertEquals(14, Async.lift(Helpers::noAwait).apply(7).join());
    }

    /** Plain lambdas (with parameters) work through lift too, not just method references. */
    @Test
    void lambdaWithParameters() {
        Async.Lifted1<CompletableFuture<Integer>, Integer> inc = Async.lift(f -> Async.await(f) + 1);
        assertEquals(42, inc.apply(later(41)).join());
    }

    @Test
    void constructorRefRejected() {
        UnsupportedOperationException e = assertThrows(UnsupportedOperationException.class,
                () -> Async.lift((Async.Fn1<Object, Object>) LiftApiTest.Box::new));
        assertTrue(e.getMessage().contains("constructor"), e.getMessage());
    }

    static final class Box {
        final Object v;
        Box(Object v) { this.v = v; }
    }

    /** Same reference lifted repeatedly hits the per-target cache. */
    @Test
    void repeatedLifts() {
        for (int i = 0; i < 30; i++) {
            int expected = i + 7;
            assertEquals("hi" + expected,
                    Async.lift(Helpers::greet).apply(i % 2 == 0 ? done("hi") : later("hi"), expected).join());
        }
    }

    /** Direct call (blocking tier) and lifted call agree. */
    @Test
    void blockingTierAgrees() {
        assertEquals(Samples.sumTwice(done(5), done("s")),
                Async.lift(Samples::sumTwice).apply(later(5), later("s")).join());
    }
}
