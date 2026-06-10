package async3.runtime;

/**
 * Renders the suspended state of a state machine with source-level variable names, by combining
 * the live {@code refs}/{@code prims} frame with the static {@code $asyncDebug} metadata table
 * the transformer emits onto each generated class (the analogue of Kotlin's
 * {@code @DebugMetadata}). Intended consumers: {@code toString}, logging of stuck futures,
 * async stack-trace dumps, and eventually a debugger plugin.
 *
 * <p>Example output:
 * <pre>
 * async3/samples/Samples.sumTwice(...) suspended at state 2 (line 23): fa = ..., x = 5
 * </pre>
 */
public final class AsyncDebug {
    private AsyncDebug() {}

    public static String describe(FutureStateMachine sm) {
        String meta;
        if (sm instanceof DelegatingStateMachine d) {
            meta = d.debugMetadata; // agent shape: one shell class, metadata carried per instance
        } else {
            try {
                meta = (String) sm.getClass().getDeclaredField("$asyncDebug").get(null);
            } catch (ReflectiveOperationException e) {
                return sm.getClass().getName() + ": state " + sm.state + " (no debug metadata)";
            }
        }
        String[] lines = meta.split("\n");
        String method = lines.length > 0 && lines[0].startsWith("method ")
                ? lines[0].substring("method ".length())
                : sm.getClass().getName();
        String prefix = "state " + sm.state;
        for (String line : lines) {
            if (!line.startsWith(prefix)) continue;
            char next = line.length() > prefix.length() ? line.charAt(prefix.length()) : '\0';
            if (next != ':' && next != ' ') continue; // "state 1" must not match "state 12"
            int colon = line.indexOf(':');
            StringBuilder sb = new StringBuilder(method).append(" suspended at ").append(line, 0, colon).append(':');
            String entries = line.substring(colon + 1).trim();
            boolean first = true;
            if (!entries.isEmpty()) {
                for (String e : entries.split(" \\| ")) {
                    sb.append(first ? " " : ", ").append(render(sm, e));
                    first = false;
                }
            }
            return sb.toString();
        }
        return method + ": state " + sm.state + " (running or completed)";
    }

    /** Entry format (from the transformer): {@code name -> refs[3] (Ljava/lang/String;)}. */
    private static String render(FutureStateMachine sm, String entry) {
        try {
            int arrow = entry.indexOf(" -> ");
            String name = entry.substring(0, arrow);
            boolean ref = entry.startsWith("refs[", arrow + 4);
            int open = entry.indexOf('[', arrow);
            int idx = Integer.parseInt(entry.substring(open + 1, entry.indexOf(']', open)));
            String desc = entry.substring(entry.indexOf('(', open) + 1, entry.lastIndexOf(')'));
            Object value = ref ? sm.refs[idx] : decode(sm.prims[idx], desc);
            return name + " = " + value;
        } catch (RuntimeException e) {
            return entry + " (unparseable)";
        }
    }

    private static Object decode(long bits, String desc) {
        return switch (desc) {
            case "J" -> bits;
            case "F" -> Float.intBitsToFloat((int) bits);
            case "D" -> Double.longBitsToDouble(bits);
            case "Z" -> bits != 0;
            case "C" -> (char) bits;
            default -> (int) bits; // I, B, S
        };
    }
}
