package p;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface C {
  Class<?> v();
  Class<?>[] vs() default {};
}
