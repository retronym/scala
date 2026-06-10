package async3.runtime;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

/**
 * Base class for generated (and hand-written) state machines.
 *
 * <p>This mirrors the ABI of {@code scala.tools.testkit.async.AsyncStateMachine}
 * ({@code state} / {@code onComplete} / {@code getCompleted} / {@code tryGet} /
 * {@code completeSuccess} / {@code completeFailure}), specialized to
 * {@link CompletableFuture} for the prototype. The transform itself only depends on these
 * member signatures, never on the concrete future type, so the design stays generic in
 * {@code F[_]}/{@code R[_]} (Future/Try, CustomFuture/Either, Node/...).
 *
 * <p>One deliberate ABI divergence: {@link #tryGet} <em>rethrows</em> a failed awaited future at
 * the suspension point instead of returning a sentinel. At the bytecode level the original
 * exception handlers of the user method are still in place around the resume point, so a plain
 * rethrow gives `try { await(f) } catch { ... }` the same semantics as the blocking tier — the
 * user's handler sees the failure. Uncaught failures reach the outermost catch-all installed by
 * the transform, which routes them to {@link #completeFailure}.
 *
 * <p>Captured state lives in a generic two-array frame ({@link #refs} / {@link #prims}): all
 * primitives are normalized to {@code long} slots, references go to {@code refs}. Slots are
 * addressed positionally (frame slot v = original local slot v; operand-stack entry j =
 * maxLocals + j), so the layout is per-state and requires no per-method field layout — the key
 * property that lets the same transform run AoT or lazily at runtime without class generation
 * beyond this one subclass.
 */
public abstract class FutureStateMachine {

    /** Current state: 0 = initial; i = suspended at await site i. */
    public int state = 0;

    /** Spilled reference slots. */
    public final Object[] refs;
    /** Spilled primitive slots; int/float/long/double all normalized to long bits. */
    public final long[] prims;

    private final CompletableFuture<Object> result = new CompletableFuture<>();

    protected FutureStateMachine(int refSlots, int primSlots) {
        this.refs = new Object[refSlots];
        this.prims = new long[primSlots];
    }

    /** Completed-successfully wrapper; distinguishes "completed with null" from "not yet completed". */
    public static final class Success {
        public final Object value;
        public Success(Object value) { this.value = value; }
    }

    private static final Success NULL_SUCCESS = new Success(null);

    /**
     * The resumable body: the transformed copy of the user method. {@code tr} is either a
     * {@link Success} or a {@link Throwable}.
     */
    public abstract void apply(Object tr);

    public final void completeSuccess(Object value) {
        result.complete(value);
    }

    public final void completeFailure(Throwable t) {
        result.completeExceptionally(t);
    }

    /**
     * Register this state machine to resume when {@code f} completes. The spill of locals/stack
     * and the {@code state} write happen-before this call; {@code whenComplete} registration
     * publishes them to the resuming thread.
     */
    public final void onComplete(CompletableFuture<Object> f) {
        f.whenComplete((v, t) -> apply(t == null ? wrap(v) : unwrap(t)));
    }

    /** Fast path: returns the result token if {@code f} is already complete, else null. */
    public final Object getCompleted(CompletableFuture<Object> f) {
        if (!f.isDone()) return null;
        try {
            return wrap(f.join());
        } catch (CompletionException e) {
            return unwrap(e);
        } catch (CancellationException e) {
            return e;
        }
    }

    /** Unwraps a result token: returns the awaited value, or rethrows the failure at the await site. */
    public final Object tryGet(Object tr) {
        if (tr instanceof Success) return ((Success) tr).value;
        throw AsyncRT.sneakyThrow((Throwable) tr);
    }

    public final CompletableFuture<Object> start() {
        // Runs synchronously on the caller thread until the first actual suspension
        // (like Kotlin's unintercepted start). Scheduling policy is a frontend concern.
        apply(NULL_SUCCESS);
        return result;
    }

    private static Success wrap(Object v) {
        return v == null ? NULL_SUCCESS : new Success(v);
    }

    private static Throwable unwrap(Throwable t) {
        return (t instanceof CompletionException && t.getCause() != null) ? t.getCause() : t;
    }
}
