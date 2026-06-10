package async3.demo;

import async3.runtime.Async;
import async3.runtime.AsyncDebug;
import async3.runtime.FutureStateMachine;
import async3.samples.InstanceSamples;
import async3.samples.NewSinkSamples;
import async3.samples.Samples;
import async3.transform.AsyncTransformer;
import async3.transform.InMemoryClassLoader;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;

/**
 * Debugger walkthrough. Run or debug this {@code main} from IntelliJ (see "Debugging in
 * IntelliJ" in the README).
 *
 * <p>Suggested breakpoints, all in {@code Samples.java} inside {@code sumTwice}:
 * <pre>
 *   int x = AsyncRT.await(fa);     // hit on entry (state 0)
 *   String s = AsyncRT.await(fb);  // hit after the first resume — inspect x in Variables
 *   return s + ":" + (x * 2);      // hit after the second resume — inspect x and s
 * </pre>
 * In scenario 2 the futures are completed manually from this thread, so every resume runs on
 * the main thread and the whole session is a single linear debug story. The Variables view
 * shows the original names (from the carried-over LocalVariableTable); {@code this} is the
 * state machine — expand {@code refs}/{@code prims} to see the raw spilled frame.
 */
public final class Demo {

    public static void main(String[] args) throws Throwable {
        Path dumpDir = Path.of("target", "transformed-classes");

        AsyncTransformer.Result samples = transformAndDump(Samples.class, dumpDir);
        AsyncTransformer.Result newSink = transformAndDump(NewSinkSamples.class, dumpDir);
        AsyncTransformer.Result instances = transformAndDump(InstanceSamples.class, dumpDir);
        System.out.println("transformed classes dumped to " + dumpDir.toAbsolutePath()
                + " (inspect with: javap -c -l -p <file>)");

        ClassLoader loader = new InMemoryClassLoader(samples, Demo.class.getClassLoader());
        Class<?> samplesT = Class.forName(samples.hostName, true, loader);

        // Shadow-debuggable lambda mode: line breakpoints set inside the lambda body below
        // bind and fire (see README "Debugging in IntelliJ"). Without this property the state
        // machine is a hidden nestmate class instead: full private access, but IDE line
        // breakpoints in the body won't bind.
        System.setProperty("async3.lambda.debuggable", "true");
        System.setProperty("async3.lambda.dump", dumpDir.toString());

        banner("0. lambda API: Async.async(() -> ...) — transform triggered by lambda cracking");
        CompletableFuture<Integer> la = later(20);
        CompletableFuture<Integer> lb = later(22);
        CompletableFuture<Integer> sum = Async.async(() -> {
            int x = Async.await(la);
            int y = Async.await(lb);   // <- breakpoint here hits in the resumed state machine
            return x + y;
        });
        System.out.println("   Async.async(() -> await(la) + await(lb)) = " + sum.join());

        // lift: derive the suspending variant of an existing method (here from another class) —
        // the runtime analogue of the annotation-driven frontend's direct/$queued method pair.
        Async.Lifted2<CompletableFuture<Integer>, CompletableFuture<String>, String> sumTwice =
                Async.lift(Samples::sumTwice);
        System.out.println("   Async.lift(Samples::sumTwice).apply(later(5), later(\"s\")) = "
                + sumTwice.apply(later(5), later("s")).join());

        banner("1. fast path: already-completed futures, runs start-to-finish on this thread");
        Object r1 = call(samplesT, null, "sumTwice$async", done(5), done("s"));
        System.out.println("   sumTwice$async(done(5), done(\"s\")) = " + join(r1));

        banner("2. real suspension, resumed by manual completion on the main thread");
        CompletableFuture<Integer> fa = new CompletableFuture<>();
        CompletableFuture<String> fb = new CompletableFuture<>();
        FutureStateMachine sm = newStateMachine(samples, loader, "sumTwice", fa, fb);
        CompletableFuture<Object> result = sm.start(); // parks at the first await
        System.out.println("   " + AsyncDebug.describe(sm));
        fa.complete(5);                                // resumes here, parks at the second await
        System.out.println("   " + AsyncDebug.describe(sm));
        fb.complete("s");                              // resumes here, completes
        System.out.println("   result = " + result.join());

        banner("3. NEW-sinking: new Pair(await(f), await(g))");
        ClassLoader nsLoader = new InMemoryClassLoader(newSink, Demo.class.getClassLoader());
        Class<?> newSinkT = Class.forName(newSink.hostName, true, nsLoader);
        Object r3 = call(newSinkT, null, "twoArgs$async", later(5), later("s"));
        System.out.println("   twoArgs$async(later(5), later(\"s\")) = " + join(r3));

        banner("4. instance method: `this` captured, private members via nestmate linkage");
        ClassLoader instLoader = new InMemoryClassLoader(instances, Demo.class.getClassLoader());
        Class<?> instT = Class.forName(instances.hostName, true, instLoader);
        Object inst = instT.getConstructor(int.class, String.class).newInstance(10, "L");
        Object r4 = call(instT, inst, "compute$async", later(3), later(4));
        System.out.println("   new InstanceSamples(10, \"L\").compute$async(later(3), later(4)) = " + join(r4));

        banner("5. the blocking tier: the same class, untransformed method, just blocks");
        System.out.println("   sumTwice(done(5), done(\"s\")) = " + call(samplesT, null, "sumTwice", done(5), done("s")));
    }

    // ------------------------------------------------------------------ plumbing

    private static AsyncTransformer.Result transformAndDump(Class<?> source, Path dir) throws Exception {
        byte[] bytes;
        try (InputStream in = source.getResourceAsStream("/" + source.getName().replace('.', '/') + ".class")) {
            bytes = in.readAllBytes();
        }
        AsyncTransformer.Result result = AsyncTransformer.transform(bytes);
        for (var e : result.allClasses().entrySet()) {
            Path p = dir.resolve(e.getKey().replace('.', '/') + ".class");
            Files.createDirectories(p.getParent());
            Files.write(p, e.getValue());
        }
        return result;
    }

    /** Instantiates the generated state machine directly, to keep a handle for AsyncDebug. */
    private static FutureStateMachine newStateMachine(AsyncTransformer.Result result, ClassLoader loader,
                                                      String method, Object... ctorArgs) throws Exception {
        String smName = result.stateMachines.keySet().stream()
                .filter(n -> n.contains("$async$" + method + "$"))
                .findFirst().orElseThrow();
        Class<?> smClass = Class.forName(smName, true, loader);
        for (Constructor<?> c : smClass.getConstructors())
            if (c.getParameterCount() == ctorArgs.length)
                return (FutureStateMachine) c.newInstance(ctorArgs);
        throw new NoSuchMethodException(smName + ".<init>/" + ctorArgs.length);
    }

    private static Object call(Class<?> cls, Object target, String name, Object... args) throws Throwable {
        for (Method m : cls.getDeclaredMethods())
            if (m.getName().equals(name)) {
                try {
                    return m.invoke(target, args);
                } catch (InvocationTargetException e) {
                    throw e.getCause();
                }
            }
        throw new NoSuchMethodException(cls.getName() + "." + name);
    }

    private static Object join(Object future) {
        return ((CompletableFuture<?>) future).join();
    }

    private static <T> CompletableFuture<T> done(T value) {
        return CompletableFuture.completedFuture(value);
    }

    private static <T> CompletableFuture<T> later(T value) {
        CompletableFuture<T> f = new CompletableFuture<>();
        CompletableFuture.delayedExecutor(20, java.util.concurrent.TimeUnit.MILLISECONDS)
                .execute(() -> f.complete(value));
        return f;
    }

    private static void banner(String s) {
        System.out.println();
        System.out.println("== " + s);
    }
}
