package async3;

import async3.runtime.Async;
import async3.runtime.AsyncRT;

import java.lang.reflect.Method;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Not a JUnit test: e2e target for the {@code -javaagent} run. Verifies the agent-prepared
 * method pair exists, and prints where the lambda body actually executes (expected: the
 * {@code lambda$main$0$asyncBody} sibling in this very class). {@link JdiAgentCheck} attaches
 * and plants a line breakpoint in the lambda body.
 */
public class AgentProbe {

    static int addOne(CompletableFuture<Integer> g) {
        return AsyncRT.await(g) + 1; // the agent adds addOne$async / addOne$asyncBody at load
    }

    public static void main(String[] args) throws Exception {
        Method entry = AgentProbe.class.getDeclaredMethod("addOne$async", CompletableFuture.class);
        System.out.println("agent entry present: " + entry.getName());
        System.out.println("addOne$async -> " + ((CompletableFuture<?>) entry.invoke(null, later(41))).join());

        for (int i = 0; i < 200; i++) {
            CompletableFuture<Integer> f = later(41);
            CompletableFuture<Integer> r = Async.async(() -> {
                int x = Async.await(f);
                StackTraceElement here = new Throwable().getStackTrace()[0];
                System.out.println("executing in " + here.getClassName() + "." + here.getMethodName());
                return x + 1;          // JdiAgentCheck plants a breakpoint on this line
            });
            System.out.println("iter " + i + " -> " + r.join());
            Thread.sleep(250);
        }
    }

    static <T> CompletableFuture<T> later(T value) {
        CompletableFuture<T> f = new CompletableFuture<>();
        CompletableFuture.delayedExecutor(50, TimeUnit.MILLISECONDS).execute(() -> f.complete(value));
        return f;
    }
}
