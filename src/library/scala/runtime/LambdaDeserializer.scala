package scala.runtime

import java.lang.invoke._

//
// class C { def foo = { () => "" } }
//
// class C {
//   def foo = { LMF(anonFun$1 };
//   private def anonFun$1 = "";
//   private <static> val lambdaDeserializer$1 = new LambdaDeserializer(MethodHandles.lookup))
//   def $deserializeLambda$(serialized: SerializedLambda) = lambdaDeserializer$1.deserializeLambda(serialized) }
// }
//
final class LambdaDeserializer(lookup: MethodHandles.Lookup) {
  def deserializeLambda(serialized: SerializedLambda): AnyRef = {
    val caller = lookup
    //    serialized.getFunctionalInterfaceClass
    val loader = lookup.lookupClass().getClassLoader
    val funcInterfacesSignature = MethodType.fromMethodDescriptorString(serialized.getFunctionalInterfaceMethodSignature, loader)
    val methodType = funcInterfacesSignature
    val instantiated = MethodType.fromMethodDescriptorString(serialized.getInstantiatedMethodType, loader)
//    val invokedType = MethodType.methodType(loader.loadClass(serialized.getFunctionalInterfaceClass.replaceAll("/", ".")))
    val implMethodSig = MethodType.fromMethodDescriptorString(serialized.getImplMethodSignature, loader)
    val implClass =loader.loadClass(serialized.getImplClass)

    val from = implMethodSig.parameterCount() - funcInterfacesSignature.parameterCount()
    val to = implMethodSig.parameterCount()
    val invokedType = implMethodSig.dropParameterTypes(from, to).changeReturnType(loader.loadClass(serialized.getFunctionalInterfaceClass.replaceAll("/", ".")))


    val implMethod: MethodHandle = {
      serialized.getImplMethodKind match {
        case MethodHandleInfo.REF_invokeStatic =>
          caller.findStatic(implClass, serialized.getImplMethodName, implMethodSig)
        case MethodHandleInfo.REF_invokeVirtual =>
          caller.findVirtual(implClass, serialized.getImplMethodName, implMethodSig)
      }
    }
    val site = LambdaMetafactory.metafactory(caller, serialized.getFunctionalInterfaceMethodName, invokedType, methodType, implMethod, methodType)
    val factory = site.getTarget
    val captures = (0 to serialized.getCapturedArgCount).map(serialized.getCapturedArg)
    val r = factory.invokeWithArguments(captures: _*)
    r
  }
}

