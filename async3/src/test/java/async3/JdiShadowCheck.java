package async3;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.Bootstrap;
import com.sun.jdi.ClassType;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.Location;
import com.sun.jdi.Method;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StackFrame;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.event.BreakpointEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.event.EventSet;
import com.sun.jdi.request.BreakpointRequest;
import com.sun.jdi.request.EventRequest;

import java.util.List;
import java.util.Map;

/**
 * Not a JUnit test; run manually against {@link JdbShadowProbe}. Simulates exactly what
 * IntelliJ's debugger does for a line breakpoint inside a lambda body: look up classes by the
 * <em>enclosing source class's name</em> (which matches across class loaders), ask each for
 * {@code locationsOfLine}, and plant a breakpoint. Verifies that the shadow-named state machine
 * is found by that procedure, that the breakpoint fires in it, and that the frame's named
 * locals are readable.
 */
public class JdiShadowCheck {

    public static void main(String[] args) throws Exception {
        String port = args.length > 0 ? args[0] : "5577";
        AttachingConnector conn = Bootstrap.virtualMachineManager().attachingConnectors().stream()
                .filter(c -> c.name().equals("com.sun.jdi.SocketAttach"))
                .findFirst().orElseThrow();
        Map<String, Connector.Argument> cargs = conn.defaultArguments();
        cargs.get("port").setValue(port);
        cargs.get("hostname").setValue("127.0.0.1");
        VirtualMachine vm = conn.attach(cargs);

        // IntelliJ step 1: classes by source-class name; the shadow appears once compiled
        ReferenceType shadow = null, original = null;
        for (int i = 0; i < 100 && shadow == null; i++) {
            for (ReferenceType t : vm.classesByName("async3.JdbShadowProbe")) {
                ClassType ct = (ClassType) t;
                boolean isShadow = ct.superclass() != null
                        && ct.superclass().name().equals("async3.runtime.FutureStateMachine");
                if (isShadow) shadow = t; else original = t;
            }
            if (shadow == null) Thread.sleep(200);
        }
        if (shadow == null) fail("shadow class named async3.JdbShadowProbe never appeared");
        System.out.println("original: " + original.name() + " loader=" + original.classLoader());
        System.out.println("shadow:   " + shadow.name() + " loader=" + shadow.classLoader()
                + " source=" + shadow.sourceName());

        // The breakpoint line as the IDE knows it from the source: take it from the dead
        // lambda$ impl method in the original class (same line table as the source). The probe
        // has two lambdas in main; the async body is the one with the highest line numbers.
        int line = -1;
        for (Method m : original.methods())
            if (m.name().startsWith("lambda$main$"))
                for (Location l : m.allLineLocations())
                    line = Math.max(line, l.lineNumber());
        if (line < 0) fail("no lambda$main$ impl method in original class");
        System.out.println("breakpoint line: " + line);

        // IntelliJ step 2: locationsOfLine on each same-named class, breakpoint per location
        List<Location> locs = shadow.locationsOfLine(line);
        if (locs.isEmpty()) fail("shadow class has no code at line " + line);
        BreakpointRequest bp = vm.eventRequestManager().createBreakpointRequest(locs.get(0));
        bp.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
        bp.enable();
        System.out.println("breakpoint set at " + locs.get(0));

        long deadline = System.currentTimeMillis() + 15_000;
        while (System.currentTimeMillis() < deadline) {
            EventSet set = vm.eventQueue().remove(2_000);
            if (set == null) continue;
            for (Event e : set) {
                if (e instanceof BreakpointEvent be) {
                    System.out.println("BREAKPOINT HIT at " + be.location()
                            + " in " + be.location().declaringType().name());
                    StackFrame frame = be.thread().frame(0);
                    try {
                        for (LocalVariable v : frame.visibleVariables())
                            System.out.println("  local " + v.name() + " = " + frame.getValue(v));
                    } catch (AbsentInformationException aie) {
                        System.out.println("  (no local variable info)");
                    }
                    set.resume();
                    vm.dispose();
                    System.out.println("OK");
                    return;
                }
            }
            set.resume();
        }
        fail("breakpoint never hit");
    }

    private static void fail(String msg) {
        System.out.println("FAIL: " + msg);
        System.exit(1);
    }
}
