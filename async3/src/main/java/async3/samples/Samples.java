package async3.samples;

import async3.runtime.AsyncRT;
import java.util.concurrent.CompletableFuture;

/**
 * Source-shape sample methods: plain synchronous code calling the {@code AsyncRT.await} marker.
 * Runnable as-is (blocking tier); input to the ASM transform, which derives a
 * {@code <name>$async} sibling returning {@code CompletableFuture}.
 *
 * <p>Static methods only for now (no {@code this} capture yet).
 */
public class Samples {

    /** Single await, result feeds an int expression. */
    public static int addOne(CompletableFuture<Integer> f) {
        return AsyncRT.await(f) + 1;
    }

    /** Two sequential awaits with locals of mixed type carried across suspension points. */
    public static String sumTwice(CompletableFuture<Integer> fa, CompletableFuture<String> fb) {
        int x = AsyncRT.await(fa);
        String s = AsyncRT.await(fb);
        return s + ":" + (x * 2);
    }

    /**
     * Awaits in expression position with a non-empty operand stack below the awaited future:
     * at the first await the stack holds [1, 2, fut]; at the second it holds [partialSum, fut].
     * This is the case the tree-level ANF transform exists to forbid.
     */
    public static int deepStack(CompletableFuture<Integer> f, CompletableFuture<Integer> g) {
        return 1 + (2 * AsyncRT.await(f)) + mul(AsyncRT.await(g), 3);
    }

    static int mul(int a, int b) {
        return a * b;
    }

    /** Await inside a loop; loop variables live across suspension points. */
    public static int loopSum(CompletableFuture<Integer>[] fs) {
        int sum = 0;
        for (int i = 0; i < fs.length; i++) {
            sum += AsyncRT.await(fs[i]);
        }
        return sum;
    }

    /** Await inside a try region: a failed future must be delivered to the user's catch handler. */
    public static String tryCatch(CompletableFuture<Integer> f) {
        try {
            int x = AsyncRT.await(f);
            return "ok:" + x;
        } catch (IllegalStateException e) {
            return "caught:" + e.getMessage();
        }
    }

    /** long/float/double locals and operands across suspension points (two-slot handling). */
    public static double mixedPrims(CompletableFuture<Double> f, long scale) {
        long l = scale * 3;
        double d = AsyncRT.await(f);
        float fl = 0.5f;
        double d2 = AsyncRT.await(f) + fl;
        return d * l + d2;
    }

    /** A reference local that is null at the first await and assigned later. */
    public static String nullLocal(CompletableFuture<Integer> f) {
        String s = null;
        int a = AsyncRT.await(f);
        if (a > 0) s = "pos";
        int b = AsyncRT.await(f);
        return s + ":" + (a + b);
    }

    /** An object constructed (fully initialized) before an await and used after it. */
    public static String initializedAcrossAwait(CompletableFuture<Integer> f) {
        StringBuilder sb = new StringBuilder("v=");
        int x = AsyncRT.await(f);
        return sb.append(x).toString();
    }
}
