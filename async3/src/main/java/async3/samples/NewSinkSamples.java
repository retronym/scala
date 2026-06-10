package async3.samples;

import async3.runtime.AsyncRT;
import java.util.concurrent.CompletableFuture;

/**
 * {@code new T(... await ...)} shapes: an uninitialized reference is on the operand stack at the
 * suspension point, which the NEW-sinking pre-pass must relocate before the spill can work.
 * This was the designated "milestone" red case.
 */
public class NewSinkSamples {

    public static final class Box {
        public final Object v;
        public Box(Object v) { this.v = v; }
        @Override public String toString() { return "Box(" + v + ")"; }
    }

    public static final class Pair {
        public final Object a, b;
        public Pair(Object a, Object b) { this.a = a; this.b = b; }
        @Override public String toString() { return "(" + a + "," + b + ")"; }
    }

    public static String simple(CompletableFuture<Integer> f) {
        return new Box(AsyncRT.await(f)).toString();
    }

    /** Two interleaved uninitialized receivers on the stack at the await. */
    public static String nested(CompletableFuture<Integer> f) {
        return new Box(new Box(AsyncRT.await(f))).toString();
    }

    /** At the second await the stack holds [uninit, uninit, arg1, future]. */
    public static String twoArgs(CompletableFuture<Integer> f, CompletableFuture<String> g) {
        return new Pair(AsyncRT.await(f), AsyncRT.await(g)).toString();
    }

    /** Result discarded; exercises the NEW;DUP;...;<init>;POP statement idiom. */
    public static String statementForm(CompletableFuture<Integer> f) {
        new Box(AsyncRT.await(f));
        return "done";
    }

    /** Await on only one path of the argument expression; <init> sits after the merge. */
    public static String conditionalArg(CompletableFuture<Integer> f, boolean flag) {
        return new Box(flag ? AsyncRT.await(f) : "n").toString();
    }
}
