package async3;

import async3.runtime.Async;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/** Not a JUnit test: the target process for {@link JdiShadowCheck}. Run with a jdwp agent. */
public class JdbShadowProbe {
    public static void main(String[] args) throws Exception {
        System.setProperty("async3.lambda.debuggable", "true");
        for (int i = 0; i < 200; i++) {
            CompletableFuture<Integer> f = new CompletableFuture<>();
            CompletableFuture.delayedExecutor(50, TimeUnit.MILLISECONDS).execute(() -> f.complete(41));
            int r = Async.<Integer>async(() -> {
                int x = Async.await(f);
                return x + 1;            // JdiShadowCheck plants a breakpoint on this line
            }).join();
            if (i % 20 == 0) System.out.println("iter " + i + " -> " + r);
            Thread.sleep(250);
        }
    }
}
