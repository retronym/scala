package ann;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface JAnn {
  Level level();
  String[] tags() default {};
  int[] sizes() default {1, 2};
}
