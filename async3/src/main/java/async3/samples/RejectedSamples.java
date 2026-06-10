package async3.samples;

import async3.runtime.AsyncRT;
import java.util.concurrent.CompletableFuture;

/**
 * Shapes the transformer must reject with a clear error rather than miscompile.
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
}
