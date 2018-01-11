package scala.runtime

import java.lang.invoke._
import java.util

import scala.annotation.varargs
import scala.collection.mutable
object LambdaDeserialize {
  @throws[Throwable]
  @varargs
  def bootstrap(lookup: MethodHandles.Lookup, invokedName: String, invokedType: MethodType, targetMethods: MethodHandle*): CallSite = {
    val exact = MethodHandleConstants.LAMBDA_DESERIALIZE_DESERIALIZE_LAMBDA.bindTo(new LambdaDeserialize(lookup, targetMethods.asInstanceOf[mutable.WrappedArray[MethodHandle]].array)).asType(invokedType)
    new ConstantCallSite(exact)
  }
  def nameAndDescriptorKey(name: String, descriptor: String): String = name + descriptor
}

final class LambdaDeserialize private(var lookup: MethodHandles.Lookup, val targetMethods: Array[MethodHandle]) {
  private var targetMethodMap: util.HashMap[String, MethodHandle] = new util.HashMap[String, MethodHandle](targetMethods.length)

  for (targetMethod <- targetMethods) {
    val info = lookup.revealDirect(targetMethod)
    val key = LambdaDeserialize.nameAndDescriptorKey(info.getName, info.getMethodType.toMethodDescriptorString)
    targetMethodMap.put(key, targetMethod)
  }

  final private val cache = new util.HashMap[String, MethodHandle]

  def deserializeLambda(serialized: SerializedLambda): Any = LambdaDeserializer.deserializeLambda(lookup, cache, targetMethodMap, serialized)
}
