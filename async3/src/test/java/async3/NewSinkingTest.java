package async3;

import async3.samples.NewSinkSamples;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The former milestone red case: {@code new T(... await ...)} with an uninitialized reference on
 * the stack at the suspension point, handled by the NEW-sinking pre-pass.
 */
class NewSinkingTest {

    static Class<?> host;

    @BeforeAll
    static void transform() {
        host = transformAndLoad(NewSinkSamples.class);
    }

    @Test
    void simple() throws Throwable {
        assertEquals("Box(5)", invokeAsync(host, "simple", done(5)));
        assertEquals("Box(5)", invokeAsync(host, "simple", later(5)));
    }

    @Test
    void nested() throws Throwable {
        assertEquals("Box(Box(5))", invokeAsync(host, "nested", done(5)));
        assertEquals("Box(Box(5))", invokeAsync(host, "nested", later(5)));
    }

    @Test
    void twoArgs() throws Throwable {
        assertEquals("(5,s)", invokeAsync(host, "twoArgs", done(5), done("s")));
        assertEquals("(5,s)", invokeAsync(host, "twoArgs", later(5), later("s")));
        assertEquals("(5,s)", invokeAsync(host, "twoArgs", done(5), later("s")));
    }

    @Test
    void statementForm() throws Throwable {
        assertEquals("done", invokeAsync(host, "statementForm", done(5)));
        assertEquals("done", invokeAsync(host, "statementForm", later(5)));
    }

    @Test
    void conditionalArg() throws Throwable {
        assertEquals("Box(5)", invokeAsync(host, "conditionalArg", done(5), true));
        assertEquals("Box(5)", invokeAsync(host, "conditionalArg", later(5), true));
        assertEquals("Box(n)", invokeAsync(host, "conditionalArg", done(5), false));
    }

    @Test
    void blockingTierAgrees() throws Throwable {
        assertEquals(NewSinkSamples.nested(done(5)), invokeAsync(host, "nested", later(5)));
        assertEquals(NewSinkSamples.twoArgs(done(5), done("s")), invokeAsync(host, "twoArgs", later(5), later("s")));
    }
}
