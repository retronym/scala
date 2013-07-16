import reflect.macros.Context, language.experimental._

object M {
  def impl(c: Context) = { c.error(c.macroApplication.pos, "triggering error expanding `m`"); c.literal("") }
  def m = macro impl
}
