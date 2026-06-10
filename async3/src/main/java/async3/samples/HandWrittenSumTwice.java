package async3.samples;

import async3.runtime.FutureStateMachine;
import java.util.concurrent.CompletableFuture;

/**
 * Phase 0: the expected shape of the transform of {@link Samples#sumTwice}, written by hand to
 * pin down the runtime ABI (state protocol, fast path, exception routing) before any ASM code.
 *
 * <p>Java source cannot GOTO into the middle of a method, so this uses the classic
 * while/switch dispatch-loop rendering (like the scalac tree transform's output). The ASM
 * transform produces the jump-based equivalent and spills into the generic refs/prims frame
 * instead of named fields; semantics are identical.
 */
public final class HandWrittenSumTwice extends FutureStateMachine {
    private final CompletableFuture<Integer> fa;
    private final CompletableFuture<String> fb;

    // captured locals (the ASM transform uses the refs/prims arrays instead)
    private int x;

    public HandWrittenSumTwice(CompletableFuture<Integer> fa, CompletableFuture<String> fb) {
        super(0, 0);
        this.fa = fa;
        this.fb = fb;
    }

    @SuppressWarnings("unchecked")
    private static CompletableFuture<Object> erase(CompletableFuture<?> f) {
        return (CompletableFuture<Object>) f;
    }

    @Override
    public void apply(Object tr) {
        try {
            while (true) {
                switch (state) {
                    case 0: {
                        Object completed = getCompleted(erase(fa));
                        state = 1;
                        if (completed != null) { tr = completed; continue; }
                        onComplete(erase(fa));
                        return;
                    }
                    case 1: {
                        x = (Integer) tryGet(tr);
                        Object completed = getCompleted(erase(fb));
                        state = 2;
                        if (completed != null) { tr = completed; continue; }
                        onComplete(erase(fb));
                        return;
                    }
                    case 2: {
                        String s = (String) tryGet(tr);
                        completeSuccess(s + ":" + (x * 2));
                        return;
                    }
                    default:
                        throw new IllegalStateException(String.valueOf(state));
                }
            }
        } catch (Throwable t) {
            completeFailure(t);
        }
    }
}
