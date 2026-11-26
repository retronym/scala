import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @interface A { Class<?> value(); }
@A(String.class) class J {}
@A(int.class) class K {}
@A(String[].class) class L {}
@A(void.class) class V {}
