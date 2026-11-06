package scala.tools.nsc.typechecker

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

import scala.reflect.internal.Flags.LOCKED
import scala.tools.testkit.BytecodeTesting

class HasMemberTest {
  /** The namer suppresses a companion's synthetic `apply` if a member of that name is locked: it takes that to be a
   *  member whose completion it is inside. On a concurrent compiler the lock may be another thread's. */
  @Test def aMemberLockedOnAnotherThreadDoesNotCount(): Unit = {
    val compiler = BytecodeTesting.newCompiler(extraArgs = "-Yparallel-typer -Yparallel-typer-threads 2")
    import compiler.global._
    new Run
    val clazz = rootMirror.EmptyPackageClass.newClass(TypeName("C"))
    clazz.setInfo(ClassInfoType(List(definitions.ObjectTpe), newScope, clazz))
    val member = clazz.newMethod(TermName("m"))
    member.setInfo(NullaryMethodType(definitions.IntTpe))
    clazz.info.decls.enter(member)
    member.setFlag(LOCKED)
    def locked = new HasMember(clazz.info, member.name, 0L, LOCKED).apply()
    val other = new Thread(() => { member.markCompleting(); assertFalse(locked) })
    assertFalse("not marked at all", locked)
    val saved = member.markCompleting()
    assertTrue("marked on this thread", locked)
    member.unmarkCompleting(saved)
    other.start(); other.join()
    assertFalse("marked on another thread", locked)
  }
}
