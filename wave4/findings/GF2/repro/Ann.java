import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME)
public @interface Ann { boolean z() default true; int i() default 0; }
