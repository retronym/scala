package async3;

import async3.samples.HandWrittenSumTwice;
import async3.samples.Samples;
import org.junit.jupiter.api.Test;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Phase 0: the hand-written state machine agrees with the blocking source-shape method,
 * pinning down the runtime ABI before any ASM is involved.
 */
class Phase0HandWrittenTest {

    @Test
    void fastPath() throws Exception {
        assertEquals("s:10", new HandWrittenSumTwice(done(5), done("s")).start().get(5, TimeUnit.SECONDS));
    }

    @Test
    void suspensionPath() throws Exception {
        assertEquals("s:10", new HandWrittenSumTwice(later(5), later("s")).start().get(5, TimeUnit.SECONDS));
    }

    @Test
    void agreesWithBlockingTier() throws Exception {
        assertEquals(Samples.sumTwice(done(5), done("s")),
                new HandWrittenSumTwice(done(5), done("s")).start().get(5, TimeUnit.SECONDS));
    }

    @Test
    void failureFailsTheResult() {
        IllegalStateException boom = new IllegalStateException("boom");
        ExecutionException e = assertThrows(ExecutionException.class,
                () -> new HandWrittenSumTwice(laterFailed(boom), later("s")).start().get(5, TimeUnit.SECONDS));
        assertSame(boom, e.getCause());
    }
}
