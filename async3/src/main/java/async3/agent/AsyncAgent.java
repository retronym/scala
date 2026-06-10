package async3.agent;

import async3.transform.AsyncTransformer;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.nio.charset.StandardCharsets;
import java.security.ProtectionDomain;

/**
 * Load-time transformer: every class that references the await markers gets, per marked method,
 * a resumable-body sibling ({@code m$asyncBody}) and a {@code m$async} entry point added —
 * see {@link AsyncTransformer#transformInPlace}. Acting at load time is what makes this shape
 * possible at all: retransformation may only replace method bodies, never add members.
 *
 * <p>What this buys over the library-only modes: the resumable body executes <em>in the host
 * class</em>, so IDE line breakpoints bind with no shadow-naming and no mode split, private
 * access needs no nestmate machinery, {@code Async.async}/{@code Async.lift} skip cracking-time
 * transformation entirely (they find the prepared entry), and the direct/lifted method pair
 * exists for every marked method on the classpath — including code you didn't build.
 *
 * <p>Usage: {@code java -javaagent:async3.jar ...} (premain), or dynamic attach (agentmain) —
 * note dynamically attached agents only affect classes loaded after attach unless paired with
 * retransformation, which this prototype does not do yet (the tier-flip phase).
 */
public final class AsyncAgent {
    private AsyncAgent() {}

    public static void premain(String agentArgs, Instrumentation inst) {
        install(inst);
    }

    public static void agentmain(String agentArgs, Instrumentation inst) {
        install(inst);
    }

    private static void install(Instrumentation inst) {
        inst.addTransformer(new ClassFileTransformer() {
            @Override
            public byte[] transform(Module module, ClassLoader loader, String className,
                                    Class<?> classBeingRedefined, ProtectionDomain protectionDomain,
                                    byte[] classfileBuffer) {
                if (className == null || loader == null) return null; // skip bootstrap classes
                if (className.startsWith("java/") || className.startsWith("jdk/")
                        || className.startsWith("sun/") || className.startsWith("com/sun/")
                        || className.startsWith("async3/runtime/") || className.startsWith("async3/transform/")
                        || className.startsWith("async3/agent/") || className.startsWith("org/objectweb/asm/"))
                    return null;
                // cheap pre-filter: a marker call needs the owner name in the constant pool
                if (indexOf(classfileBuffer, MARKER) < 0) return null;
                try {
                    return AsyncTransformer.transformInPlace(classfileBuffer); // null = unchanged
                } catch (Throwable t) {
                    System.err.println("[async3-agent] leaving " + className + " untransformed: " + t);
                    return null;
                }
            }
        });
    }

    /** Matches both marker owners: "async3/runtime/AsyncRT" contains this as a prefix. */
    private static final byte[] MARKER = "async3/runtime/Async".getBytes(StandardCharsets.ISO_8859_1);

    private static int indexOf(byte[] haystack, byte[] needle) {
        outer:
        for (int i = 0; i <= haystack.length - needle.length; i++) {
            for (int j = 0; j < needle.length; j++)
                if (haystack[i + j] != needle[j]) continue outer;
            return i;
        }
        return -1;
    }
}
