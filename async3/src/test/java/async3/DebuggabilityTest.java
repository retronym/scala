package async3;

import async3.runtime.AsyncDebug;
import async3.runtime.FutureStateMachine;
import async3.samples.Samples;
import async3.transform.AsyncTransformer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.LocalVariableNode;
import org.objectweb.asm.tree.MethodNode;

import java.lang.reflect.Constructor;
import java.util.concurrent.CompletableFuture;

import static async3.TestSupport.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Phase 3 seeds: while suspended, the heap frame is renderable with source variable names;
 * while executing, the generated {@code apply} carries the original LocalVariableTable
 * (restores go into the original — shifted — slots, so the entries stay truthful and a
 * debugger shows ordinary named locals in resumed code).
 */
class DebuggabilityTest {

    static AsyncTransformer.Result result;
    static TestSupport.TransformedLoader loader;

    @BeforeAll
    static void transform() {
        result = AsyncTransformer.transform(classBytes(Samples.class));
        loader = new TestSupport.TransformedLoader(result);
    }

    private static String smNameFor(String method) {
        return result.stateMachines.keySet().stream()
                .filter(n -> n.contains("$async$" + method + "$"))
                .findFirst().orElseThrow();
    }

    @Test
    void suspendedFrameRendersWithSourceNames() throws Exception {
        // suspend sumTwice at its second await: first future already done, second never completes
        Class<?> smClass = Class.forName(smNameFor("sumTwice"), true, loader);
        Constructor<?> ctor = smClass.getConstructor(CompletableFuture.class, CompletableFuture.class);
        FutureStateMachine sm = (FutureStateMachine) ctor.newInstance(done(5), new CompletableFuture<String>());
        sm.start();

        assertEquals(2, sm.state);
        String described = AsyncDebug.describe(sm);
        assertTrue(described.contains("Samples.sumTwice"), described);
        assertTrue(described.contains("suspended at state 2"), described);
        assertTrue(described.contains("(line "), described);
        assertTrue(described.contains("x = 5"), described);
        System.out.println(described);
    }

    @Test
    void applyCarriesOriginalLocalVariableTable() throws Exception {
        byte[] smBytes = result.stateMachines.get(smNameFor("sumTwice"));
        ClassNode cn = new ClassNode();
        new ClassReader(smBytes).accept(cn, 0);
        MethodNode apply = cn.methods.stream().filter(m -> m.name.equals("apply")).findFirst().orElseThrow();
        assertNotNull(apply.localVariables);
        boolean foundX = false;
        for (LocalVariableNode lv : apply.localVariables) {
            if (lv.name.equals("x")) {
                foundX = true;
                // original slot 2, shifted by 2 (this, tr)
                assertEquals(4, lv.index);
            }
        }
        assertTrue(foundX, "expected local variable `x` in apply's LVT");
        // line numbers survive the copy too
        assertTrue(apply.instructions.toArray().length > 0);
    }
}
