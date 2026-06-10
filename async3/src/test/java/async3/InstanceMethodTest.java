package async3;

import async3.samples.InstanceSamples;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Instance methods: `this` captured in the frame; private field reads/writes and private
 * method calls from the generated state machine work via nestmate linkage.
 */
class InstanceMethodTest {

    static Class<?> host;

    @BeforeAll
    static void transform() {
        host = transformAndLoad(InstanceSamples.class);
    }

    @Test
    void fastPath() throws Throwable {
        Object inst = newInstance(host, 10, "L");
        // x=3, label -> "L*", y=4, add(7) = 17
        assertEquals("L*:17", invokeAsyncOn(inst, "compute", done(3), done(4)));
    }

    @Test
    void suspensionPath() throws Throwable {
        Object inst = newInstance(host, 10, "L");
        assertEquals("L*:17", invokeAsyncOn(inst, "compute", later(3), later(4)));
    }

    /** Field mutation across awaits persists on the instance between calls. */
    @Test
    void statefulInstance() throws Throwable {
        Object inst = newInstance(host, 100, "S");
        assertEquals("S*:107", invokeAsyncOn(inst, "compute", later(3), done(4)));
        assertEquals("S**:107", invokeAsyncOn(inst, "compute", done(3), later(4)));
    }

    @Test
    void blockingTierAgrees() throws Throwable {
        assertEquals(new InstanceSamples(10, "L").compute(done(3), done(4)),
                invokeAsyncOn(newInstance(host, 10, "L"), "compute", later(3), later(4)));
    }
}
