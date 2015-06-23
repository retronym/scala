package scala.compat.java8.runtime;
/**
 * This class is only intended to be called by synthetic <code>$deserializeLambda$</code> method that the Scala 2.12
 * compiler will add to classes hosting lambdas.
 * <p>
 * It is not intended to be consumed directly.
 */
public  class LambdaDeserializer {
  /**
   * Deserialize a lambda by calling <code>LambdaMetafactory.altMetafactory</code> to spin up a lambda class
   * and instantiating this class with the captured arguments.
   * <p>
   * A cache may be provided to ensure that subsequent deserialization of the same lambda expression
   * is cheap, it amounts to a reflective call to the constructor of the previously created class.
   * However, deserialization of the same lambda expression is not guaranteed to use the same class,
   * concurrent deserialization of the same lambda expression may spin up more than one class.
   * <p>
   * Assumptions:
   *  - No additional marker interfaces are required beyond <code>{java.io,scala.}Serializable</code>. These are
   *    not stored in <code>SerializedLambda</code>, so we can't reconstitute them.
   *  - No additional bridge methods are passed to <code>altMetafactory</code>. Again, these are not stored.
   * <p>
   * @param lookup      The factory for method handles. Must have access to the implementation method, the
   *                    functional interface class, and <code>java.io.Serializable</code> or <code>scala.Serializable</code> as
   *                    required.
   * @param cache       A cache used to avoid spinning up a class for each deserialization of a given lambda. May be <code>null</code>
   * @param serialized  The lambda to deserialize. Note that this is typically created by the <code>readResolve</code>
   *                    member of the anonymous class created by <code>LambdaMetaFactory</code>.
   * @return            An instance of the functional interface
   */
  static public  java.lang.Object deserializeLambda (java.lang.invoke.MethodHandles.Lookup lookup, java.util.Map<java.lang.String, java.lang.invoke.MethodHandle> cache, java.lang.invoke.SerializedLambda serialized) { throw new RuntimeException(); }
  static private  java.lang.String ScalaSerializable () { throw new RuntimeException(); }
  static private  java.lang.String JavaIOSerializable () { throw new RuntimeException(); }
  static private  java.lang.invoke.MethodHandle findMember (java.lang.invoke.MethodHandles.Lookup lookup, int kind, java.lang.Class<?> owner, java.lang.String name, java.lang.invoke.MethodType signature) { throw new RuntimeException(); }
}
