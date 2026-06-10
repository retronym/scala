package async3;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.Bootstrap;
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
 * Not a JUnit test; run manually against {@link AgentProbe} started with {@code -javaagent}.
 * Replays IntelliJ's procedure for a line breakpoint inside a lambda body — look up the
 * enclosing class by name, take {@code locationsOfLine}, set breakpoints on all of them — and
 * verifies that with the agent shape (resumable body as a sibling method of the host class)
 * the hit lands in {@code lambda$main$0$asyncBody}, in the host class, with no shadow classes
 * and no special modes.
 */
public class JdiAgentCheck {

    public static void main(String[] args) throws Exception {
        String port = args.length > 0 ? args[0] : "5579";
        AttachingConnector conn = Bootstrap.virtualMachineManager().attachingConnectors().stream()
                .filter(c -> c.name().equals("com.sun.jdi.SocketAttach"))
                .findFirst().orElseThrow();
        Map<String, Connector.Argument> cargs = conn.defaultArguments();
        cargs.get("port").setValue(port);
        cargs.get("hostname").setValue("127.0.0.1");
        VirtualMachine vm = conn.attach(cargs);

        ReferenceType probe = null;
        for (int i = 0; i < 100 && probe == null; i++) {
            List<ReferenceType> types = vm.classesByName("async3.AgentProbe");
            if (!types.isEmpty()) probe = types.get(0);
            else Thread.sleep(200);
        }
        if (probe == null) fail("async3.AgentProbe never loaded");

        // the source line of the lambda body, as the IDE would know it
        int line = -1;
        for (Method m : probe.methods())
            if (m.name().startsWith("lambda$main$") && !m.name().endsWith("$asyncBody")) {
                try {
                    for (Location l : m.allLineLocations())
                        line = Math.max(line, l.lineNumber());
                } catch (AbsentInformationException e) {
                    // the generated $async entry point is synthetic, no line table — skip
                }
            }
        if (line < 0) fail("no lambda$main$ impl method found");
        System.out.println("breakpoint line: " + line);

        // IntelliJ sets a request for every location of the line — here that covers both the
        // dead original lambda$main$0 and the live lambda$main$0$asyncBody sibling
        List<Location> locs = probe.locationsOfLine(line);
        if (locs.isEmpty()) fail("no locations at line " + line);
        for (Location l : locs) {
            System.out.println("location: " + l + " in " + l.method().name());
            BreakpointRequest bp = vm.eventRequestManager().createBreakpointRequest(l);
            bp.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD);
            bp.enable();
        }

        long deadline = System.currentTimeMillis() + 15_000;
        while (System.currentTimeMillis() < deadline) {
            EventSet set = vm.eventQueue().remove(2_000);
            if (set == null) continue;
            for (Event e : set) {
                if (e instanceof BreakpointEvent be) {
                    String method = be.location().method().name();
                    System.out.println("BREAKPOINT HIT in " + be.location().declaringType().name()
                            + "." + method + " at line " + be.location().lineNumber());
                    StackFrame frame = be.thread().frame(0);
                    try {
                        for (LocalVariable v : frame.visibleVariables())
                            System.out.println("  local " + v.name() + " = " + frame.getValue(v));
                    } catch (AbsentInformationException aie) {
                        System.out.println("  (no local variable info)");
                    }
                    set.resume();
                    vm.dispose();
                    if (!method.endsWith("$asyncBody")) fail("hit the dead original method, not the sibling");
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
