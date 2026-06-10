package async3.samples;

import async3.runtime.AsyncRT;
import java.util.concurrent.CompletableFuture;

/**
 * Instance methods: {@code this} is captured like any other local (slot 0 of the entry frame).
 * Private member access from the resumed code exercises the nestmate linkage the transformer
 * sets up between the host class and its generated state machines.
 */
public class InstanceSamples {

    private final int base;     // private field read via private method, from resumed code
    private String label;       // private field read AND written across suspension points

    public InstanceSamples(int base, String label) {
        this.base = base;
        this.label = label;
    }

    public String compute(CompletableFuture<Integer> f, CompletableFuture<Integer> g) {
        int x = AsyncRT.await(f);
        label = label + "*";
        int y = AsyncRT.await(g);
        return label + ":" + add(x + y);
    }

    private int add(int v) {
        return base + v;
    }
}
