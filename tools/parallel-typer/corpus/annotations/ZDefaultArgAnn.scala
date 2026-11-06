package annots

// An annotation class with a default argument, used from another file. Whether its parameter has `@defaultArg` yet
// depends on whether this file's body has been typed; the pickles of the users must not. Named to sort after its
// users, where the serial compiler also uses the default getter (before them, it inlines the default: O3).
class DefaultArgAnn(cause: String = "", n: Int = 1) extends scala.annotation.StaticAnnotation
