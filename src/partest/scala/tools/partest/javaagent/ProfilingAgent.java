/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Grzegorz Kossakowski
 */

package scala.tools.partest.javaagent;

import java.lang.instrument.Instrumentation;
import java.lang.instrument.UnmodifiableClassException;

/**
 * Profiling agent that instruments byte-code to insert calls to
 * {@link scala.tools.partest.instrumented.Profiler#methodCalled(String, String, String)}
 * by using ASM library for byte-code manipulation.
 */
public class ProfilingAgent {

    private static final String GLOBAL_INSTR = "globalInstr";

    public static void premain(String args, Instrumentation inst) throws UnmodifiableClassException {
        saveGlobalInst(inst);

        // NOTE: we are adding transformer that won't be applied to classes that are already loaded
        // This should be ok because premain should be executed before main is executed so Scala library
        // and the test-case itself won't be loaded yet. We rely here on the fact that ASMTransformer does
        // not depend on Scala library. In case our assumptions are wrong we can always insert call to
        // inst.retransformClasses.
        inst.addTransformer(new ASMTransformer(), false);
    }

    /** Get the actual (shallow) size of an object. Technique described http://www.javamex.com/tutorials/memory/instrumentation.shtml */
    public static long getObjectSize(Object obj) {
        Instrumentation globalInstr = getGlobalInst();
        if (globalInstr == null) {
            System.out.println("Agent not initialized");
            return -1L;
        }

        return globalInstr.getObjectSize(obj);
    }

    // Due to classloader shenanigans, we can't just put this in an static variable.
    // So we nefariously fake a JVM wide static variable with the properties map.
    private static void saveGlobalInst(Instrumentation inst) {System.getProperties().put(GLOBAL_INSTR, inst);}

    private static Instrumentation getGlobalInst() {return (Instrumentation) System.getProperties().get(GLOBAL_INSTR);}
}
