public class Vector<E> {
    public void elements() {
        new C<E>() {
        };
    }

    private class PrivateInner {
    }
    class SubPrivateInner extends PrivateInner {
    }
}

class C<E> {}
