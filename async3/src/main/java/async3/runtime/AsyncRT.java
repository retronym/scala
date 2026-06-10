package async3.runtime;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

/**
 * The await marker.
 *
 * <p>A call to {@link #await} in bytecode is the unambiguous marker of a suspension point. The
 * default implementation here is the synchronous "tier 0": it just blocks. A method that calls it
 * is fully functional without any transformation. The ASM transform
 * ({@code async3.transform.AsyncTransformer}) replaces the call instruction with a
 * spill/suspend/resume sequence in a derived {@code <method>$async} variant; in the
 * runtime-deferred mode the same rewrite can be applied lazily once profiling shows the blocking
 * version is a bottleneck.
 */
public final class AsyncRT {
    private AsyncRT() {}

    /**
     * Tier 0 semantics: block until the future completes; rethrow the original failure (not the
     * CompletionException wrapper) so that exception semantics match the transformed version,
     * where {@code tryGet} rethrows the failure at the suspension point.
     */
    public static <T> T await(CompletableFuture<T> future) {
        try {
            return future.join();
        } catch (CompletionException e) {
            throw sneakyThrow(e.getCause() != null ? e.getCause() : e);
        } catch (CancellationException e) {
            throw e;
        }
    }

    @SuppressWarnings("unchecked")
    static <T extends Throwable> RuntimeException sneakyThrow(Throwable t) throws T {
        throw (T) t;
    }
}
