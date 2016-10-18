package scala.reflect.internal.jep261

import java.net.URI
import java.util
import java.util.Optional
import javax.naming.ConfigurationException


// Long name to avoid confusion with existing meaning of "module" in scalac
final case class Jep261Module(name: String, requirements: List[ModuleRequirement], exports: List[String])

final case class ModuleRequirement(name: String)

object Test {
  import java.lang.module._
  val nullModuleReader = new ModuleReader {
    override def close(): Unit = ()
    override def find(name: String): Optional[URI] = Optional.empty()
  }
  val b = new ModuleDescriptor.Builder("com.acme.mod1").build()
  val sourceModules = util.Set.of(
    new ModuleReference(new ModuleDescriptor.Builder("com.acme.mod1").build(), new URI("file:/mod1"), () => nullModuleReader),
    new ModuleReference(new ModuleDescriptor.Builder("com.acme.mod2").requires("com.acme.mod1").requires("java.xml").build(), new URI("file:/mod1"), () => nullModuleReader)
  )
  object FromSourceModuleFinder extends ModuleFinder {
    override def findAll(): util.Set[ModuleReference] = sourceModules
    override def find(name: String): Optional[ModuleReference] = sourceModules.stream().filter(x => x.descriptor().name() == name).findFirst()
  }
  Configuration.empty().resolveRequires(ModuleFinder.ofSystem(), ModuleFinder.of(), util.List.of("comp.acme.mod2"))
}