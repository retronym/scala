import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME)
public @interface Ann {
  boolean z() default true;
  byte b() default 0;
  short sh() default 0;
  char c() default 'a';
  int i() default 0;
  long l() default 0L;
  float f() default 0f;
  double d() default 0d;
  String s() default "";
  Class<?> cls() default Object.class;
  Color e() default Color.Red;
  int[] is() default {};
  String[] ss() default {};
  Color[] es() default {};
  Nested n() default @Nested;
  Nested[] ns() default {};
}
