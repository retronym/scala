package async3.runtime;

import java.lang.invoke.MethodHandle;

/**
 * The state machine used by the in-place ("agent") shape: a single runtime class for all
 * transformed methods. The resumable body lives as a static sibling method of the host class
 * ({@code m$asyncBody(FutureStateMachine, Object)}); this shell just delegates to it through a
 * MethodHandle that the generated entry point loads as an LDC constant. No per-method class
 * generation, no nest tricks (the body is host code), and IDE breakpoints bind naturally
 * because the executing bytecode lives in the class the source lines belong to.
 */
public final class DelegatingStateMachine extends FutureStateMachine {

    private final MethodHandle body; // exactly (FutureStateMachine, Object) -> void
    /** Per-method frame-layout metadata; surfaced by {@link AsyncDebug#describe}. */
    public final String debugMetadata;

    public DelegatingStateMachine(int refSlots, int primSlots, MethodHandle body, String debugMetadata) {
        super(refSlots, primSlots);
        this.body = body;
        this.debugMetadata = debugMetadata;
    }

    @Override
    public void apply(Object tr) {
        try {
            body.invokeExact((FutureStateMachine) this, tr);
        } catch (Throwable t) {
            // The body routes all user-code failures to completeFailure itself; anything
            // escaping here is a linkage-level problem.
            throw AsyncRT.sneakyThrow(t);
        }
    }
}
