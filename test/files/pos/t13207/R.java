import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

public class R {
  @Target(ElementType.TYPE_USE) @interface Ann { }

  public void receiver(R this, int x) { }
  public void only(R this) { }
  public void annotated(@Ann R this, String s) { }
  public <T> T generic(R this, T t, T... ts) { return t; }

  public class Inner {
    public Inner(R R.this) { }
    public Inner(R R.this, int x) { }
    public Inner(@Ann R R.this, String s) { }
    public void m(Inner this, int x) { }
    public void n(R.Inner this) { }
  }

  public static class G<T> {
    public void m(G<T> this, T t) { }
    public class I {
      public I(G<T> G.this, T t) { }
    }
  }
}
