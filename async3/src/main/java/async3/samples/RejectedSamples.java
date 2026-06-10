package async3.samples;

import async3.runtime.AsyncRT;
import java.util.concurrent.CompletableFuture;

/**
 * Shapes the transformer must (for now) reject with a clear error rather than miscompile.
 * One class per shape so each rejection can be exercised independently (the transformer
 * fails fast on the first offending method of a class).
 */
public final class RejectedSamples {
    private RejectedSamples() {}

    /** Suspension while holding a monitor: illegal, must be detected and refused. */
    public static final class Monitor {
        public static int underMonitor(CompletableFuture<Integer> f) {
            Object lock = new Object();
            synchronized (lock) {
                return AsyncRT.await(f);
            }
        }
    }

    public static final class Box {
        public final Object v;
        public Box(Object v) { this.v = v; }
    }

    /**
     * Uninitialized object on the operand stack at the suspension point
     * ({@code NEW; DUP; ...await...; INVOKESPECIAL <init>}): cannot be spilled to the heap.
     * Kotlin sinks the NEW/DUP past the suspension; until that lands here, reject.
     */
    public static final class UninitNew {
        public static Object newWithAwaitArg(CompletableFuture<Integer> f) {
            return new Box(AsyncRT.await(f));
        }
    }
}
