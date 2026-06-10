package async3.transform;

import java.util.Map;

/**
 * Defines the patched host class and its generated state machines, child-first for exactly
 * those names so the transformed host shadows the original on the classpath. Every
 * {@code defineClass} here runs the JVM verifier over the generated code.
 */
public final class InMemoryClassLoader extends ClassLoader {
    private final Map<String, byte[]> classes;

    public InMemoryClassLoader(AsyncTransformer.Result result, ClassLoader parent) {
        this(result.allClasses(), parent);
    }

    public InMemoryClassLoader(Map<String, byte[]> classes, ClassLoader parent) {
        super(parent);
        this.classes = Map.copyOf(classes);
    }

    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        synchronized (getClassLoadingLock(name)) {
            Class<?> c = findLoadedClass(name);
            if (c == null) {
                byte[] b = classes.get(name);
                c = b != null ? defineClass(name, b, 0, b.length) : super.loadClass(name, false);
            }
            if (resolve) resolveClass(c);
            return c;
        }
    }
}
