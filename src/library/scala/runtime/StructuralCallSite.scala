package scala.runtime

import java.lang.invoke._
import java.lang.ref.SoftReference
import java.lang.reflect.Method

final class StructuralCallSite private(val callType: MethodType) {
  private var _parameterTypes = callType.parameterArray
  private var cache: SoftReference[MethodCache] =  new SoftReference(new EmptyMethodCache)
  def get: MethodCache = {
    var cache = this.cache.get
    if (cache == null) {
      cache = new EmptyMethodCache
      this.cache = new SoftReference(cache)
    }
    cache
  }
  def find(receiver: Class[_]): Method = get.find(receiver)
  def add(receiver: Class[_], m: Method): Method = {
    cache = new SoftReference(get.add(receiver, m))
    m
  }
  def parameterTypes: Array[Class[_]] = _parameterTypes
}
object StructuralCallSite {
  def bootstrap(lookup: MethodHandles.Lookup, invokedName: String, invokedType: MethodType, reflectiveCallType: MethodType): CallSite = {
    val structuralCallSite = new StructuralCallSite(reflectiveCallType)
    new ConstantCallSite(MethodHandles.constant(classOf[StructuralCallSite], structuralCallSite))
  }
}
