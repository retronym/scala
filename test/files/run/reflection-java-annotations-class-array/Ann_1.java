import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface Ann_1 {
  Class<?> c1() default String.class;
  Class<?> c2() default String[].class;
  Class<?> c3() default int[].class;
  Class<?> c4() default String[][].class;
  Class<?> c5() default int.class;
  Class<?> c6() default void.class;
  Class<?>[] cs() default {String.class, String[].class, long[][].class, double.class};
}
