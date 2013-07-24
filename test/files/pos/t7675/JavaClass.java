package javapkg;

import scalapkg.SomeScala.*;

public class JavaClass {
    public void useInner0(Object probablyInner) {
        InnerClass inner = (InnerClass) probablyInner;
        inner.printStr();
    }

    public void useInner1(InnerClass inner) {
        inner.printStr();
    }
}
