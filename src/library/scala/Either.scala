/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import language.implicitConversions
import scala.Either.{RightProjection, LeftProjection}

/** Represents a value of one of two possible types (a disjoint union.)
 *  Instances of Either are either an instance of [[scala.Left]] or [[scala.Right]].
 *
 *  A common use of Either is as an alternative to [[scala.Option]] for dealing
 *  with possible missing values.  In this usage, [[scala.None]] is replaced
 *  with a [[scala.Left]] which can contain useful information.
 *  [[scala.Right]] takes the place of [[scala.Some]].  Convention dictates
 *  that Left is used for failure and Right is used for success. Either is
 *  __right biased__; the methods on Either make it convenient to work with the
 *  Right value.
 *
 *  For example, you could use ``Either[String, Int]`` to detect whether a
 *  received input is a String or an Int.
 *
 *  {{{
 *  val in = Console.readLine("Type Either a string or an Int: ")
 *  val result: Either[String,Int] = try {
 *      Right(in.toInt)
 *    } catch {
 *      case e: Exception =>
 *        Left(in)
 *  }
 *
 *  println( result match {
 *    case Right(x) => "You passed me the Int: " + x + ", which I will increment. " + x + " + 1 = " + (x+1)
 *    case Left(x) => "You passed me the String: " + x
 *  })
 *  }}}
 *
 *  A ''projection'' can be used to selectively operate on a value of type Either,
 *  depending on whether it is of type Left or Right. For example, to transform an
 *  Either using a function, in the case where it's a Left, one can first apply
 *  the `left` projection and invoke `map` on that projected Either. If a `right`
 *  projection is applied to that Left, the original Left is returned, unmodified.
 *
 *  {{{
 *  val l: Either[String, Int] = Left("flower")
 *  val r: Either[String, Int] = Right(12)
 *  l.left.map(_.size): Either[Int, Int] // Left(6)
 *  r.left.map(_.size): Either[Int, Int] // Right(12)
 *  l.right.map(_.toDouble): Either[String, Double] // Left("flower")
 *  r.right.map(_.toDouble): Either[String, Double] // Right(12.0)
 *  }}}
 *
 *  Like with other types which define a `map` method, the same can be achieved
 *  using a for-comprehension:
 *  {{{
 *  for (s <- l.left) yield s.size // Left(6)
 *  }}}
 *
 *  To support multiple projections as generators in for-comprehensions, the Either
 *  type also defines a `flatMap` method.
 *
 *  Because Either is right-biased, code that uses the Right projection can be shortened
 *  to directly use the methods of Either:
 *
 *  {{{
 *  r.right.map(_ * 2).right.flatMap(x => Right(x * x)) // long form
 *  r.map(_ * 2).flatMap(x => Right(x * x))             // short form
 *  }}}
 *
 *  @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 *  @version 1.0, 11/10/2008
 *  @since 2.7
 */
sealed abstract class Either[+A, +B] {
  /**
   * Projects this `Either` as a `Left`.
   */
  final def left: LeftProjection[A, B] = LeftProjection(this)

  /**
   * Projects this `Either` as a `Right`.
   */
  final def right: RightProjection[A, B] = RightProjection(this)

  /**
   * Applies `fa` if this is a `Left` or `fb` if this is a `Right`.
   *
   * @example {{{
   * val result: Either[Exception, Value] = possiblyFailingOperation()
   * log(result.fold(
   *   ex => "Operation failed with " + ex,
   *   v => "Operation produced value: " + v
   * ))
   * }}}
   *
   * @param fa the function to apply if this is a `Left`
   * @param fb the function to apply if this is a `Right`
   * @return the results of applying the function
   */
  final def fold[X](fa: A => X, fb: B => X): X = this match {
    case Left(a) => fa(a)
    case Right(b) => fb(b)
  }

  /**
   * If this is a `Left`, then return the left value in `Right` or vice versa.
   *
   * @example {{{
   * val l: Either[String, Int] = Left("left")
   * val r: Either[Int, String] = l.swap // Result: Right("left")
   * }}}
   */
  final def swap: Either[B, A] = this match {
    case Left(a) => Right(a)
    case Right(b) => Left(b)
  }

  /**
   * Returns the value from this `Right` or throws
   * `Predef.NoSuchElementException` if this is a `Left`.
   *
   * {{{
   * Right(12).get // 12
   * Left(12).get // NoSuchElementException
   * }}}
   *
   * @throws Predef.NoSuchElementException if this is `Left`.
   */
  final def get: B = this match {
    case Left(_) => throw new NoSuchElementException("Either.get on Left")
    case Right(a) => a
  }

  /**
   * Executes the given side-effecting function if this is a `Right`.
   *
   * {{{
   * Right(12).foreach(x => println(x)) // prints "12"
   * Left(12).foreach(x => println(x))  // doesn't print
   * }}}
   * @param f The side-effecting function to execute.
   */
  @inline final def foreach[U](f: B => U): Unit = this match {
    case Left(_) => ()
    case Right(b) => f(b)
  }

  /**
   * Returns the value from this `Right` or the given argument if this is a
   * `Left`.
   *
   * {{{
   * Right(12).getOrElse(17) // 12
   * Left(12).getOrElse(17)  // 17
   * }}}
   */
  @inline final def getOrElse[BB >: B](or: => BB): BB = this match {
    case Left(_) => or
    case Right(b) => b
  }

  /**
   * Returns `true` if `Left` or returns the result of the application of
   * the given function to the `Right` value.
   *
   * {{{
   * Right(12).forall(_ > 10) // true
   * Right(7).forall(_ > 10)  // false
   * Left(12).forall(_ > 10)  // true
   * }}}
   */
  @inline final def forall(f: B => Boolean): Boolean = this match {
    case Left(_) => true
    case Right(b) => f(b)
  }

  /**
   * Returns `false` if `Left` or returns the result of the application of
   * the given function to the `Right` value.
   *
   * {{{
   * Right(12).exists(_ > 10)  // true
   * Right(7).exists(_ > 10)   // false
   * Left(12).exists(_ > 10)   // false
   * }}}
   */
  @inline final def exists(f: B => Boolean): Boolean = this match {
    case Left(_) => false
    case Right(b) => f(b)
  }

  /**
   * Binds the given function across `Right`.
   *
   * @param f The function to bind across `Right`.
   */
  @inline final def flatMap[AA >: A, Y](f: B => Either[AA, Y]): Either[AA, Y] = this match {
    case Left(a) => Left(a)
    case Right(b) => f(b)
  }

  /**
   * The given function is applied if this is a `Right`.
   *
   * {{{
   * Right(12).map(x => "flower") // Result: Right("flower")
   * Left(12).map(x => "flower")  // Result: Left(12)
   * }}}
   */
  @inline final def map[Y](f: B => Y): Either[A, Y] = this match {
    case Left(a) => Left(a)
    case Right(b) => Right(f(b))
  }

  // filter omitted intentionally, as it doesn't work well in a for-comprehension.

  /**Returns a `Seq` containing the `Right` value if
   * it exists or an empty `Seq` if this is a `Left`.
   *
   * {{{
   * Right(12).toSeq // Seq(12)
   * Left(12).toSeq // Seq()
   * }}}
   */
  final def toSeq: Seq[B] = this match {
    case Left(_) => Seq.empty
    case Right(b) => Seq(b)
  }

  /**Returns a `Some` containing the `Right` value
   * if it exists or a `None` if this is a `Left`.
   *
   * {{{
   * Right(12).toOption // Some(12)
   * Left(12).toOption // None
   * }}}
   */
  final def toOption: Option[B] = this match {
    case Left(_) => None
    case Right(b) => Some(b)
  }

  /**
   * Joins an `Either` through `Right`.
   *
   * This method requires that the right side of this Either is itself an
   * Either type. That is, this must be some type like: {{{
   * Either[A, Either[A, C]]
   * }}} (which respects the type parameter bounds, shown below.)
   *
   * If this instance is a Right[Either[A, C]] then the contained Either[A, C]
   * will be returned, otherwise this value will be returned unmodified.
   *
   * @example {{{
   * Right[String, Either[String, Int]](Right(12)).joinRight // Result: Right(12)
   * Right[String, Either[String, Int]](Left("flower")).joinRight // Result: Left("flower")
   * Left[String, Either[String, Int]]("flower").joinRight // Result: Left("flower")
   * }}}
   *
   * This method, and `joinLeft`, are analogous to `Option#flatten`
   */
  final def joinRight[A1 >: A, B1 >: B, C](implicit ev: B1 <:< Either[A1, C]): Either[A1, C] = this match {
    case Left(a)  => Left(a)
    case Right(b) => b
  }

  /**
   * Joins an `Either` through `Left`.
   *
   * This method requires that the left side of this Either is itself an
   * Either type. That is, this must be some type like: {{{
   * Either[Either[C, B], B]
   * }}} (which respects the type parameter bounds, shown below.)
   *
   * If this instance is a Left[Either[C, B]] then the contained Either[C, B]
   * will be returned, otherwise this value will be returned unmodified.
   *
   * {{{
   * Left[Either[Int, String], String](Right("flower")).joinLeft // Result: Right("flower")
   * Left[Either[Int, String], String](Left(12)).joinLeft // Result: Left(12)
   * Right[Either[Int, String], String]("daisy").joinLeft // Result: Right("daisy")
   * }}}
   *
   * This method, and `joinRight`, are analogous to `Option#flatten`
   */
  final def joinLeft[A1 >: A, B1 >: B, C](implicit ev: A1 <:< Either[C, B1]): Either[C, B1] = this match {
    case Left(a)  => a
    case Right(b) => Right(b)
  }

  /**
   * Returns `true` if this is a `Left`, `false` otherwise.
   *
   * {{{
   * Left("tulip").isLeft // true
   * Right("venus fly-trap").isLeft // false
   * }}}
   */
  def isLeft: Boolean

  /**
   * Returns `true` if this is a `Right`, `false` otherwise.
   *
   * {{{
   * Left("tulip").isRight // false
   * Right("venus fly-trap").isRight // true
   * }}}
   */
  def isRight: Boolean
}

/**
 * The left side of the disjoint union, as opposed to the [[scala.Right]] side.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 11/10/2008
 */
final case class Left[+A, +B](a: A) extends Either[A, B] {
  def isLeft = true
  def isRight = false
}

/**
 * The right side of the disjoint union, as opposed to the [[scala.Left]] side.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
 * @version 1.0, 11/10/2008
 */
final case class Right[+A, +B](b: B) extends Either[A, B] {
  def isLeft = false
  def isRight = true
}

object Either {

  /**
   * Allows use of a ``merge`` method to extract values from Either instances
   * regardless of whether they are Left or Right.
   *
   * {{{
   * val l = Left(List(1)): Either[List[Int], Vector[Int]]
   * val r = Right(Vector(1)): Either[List[Int], Vector[Int]]
   * l.merge: Seq[Int] // List(1)
   * r.merge: Seq[Int] // Vector(1)
   * }}}
   */
  implicit class MergeableEither[A](x: Either[A, A]) {
    def merge: A = x match {
      case Left(a)  => a
      case Right(a) => a
    }
  }
  @deprecated("use MergeableEither instead", "2.10")
  def either2mergeable[A](x: Either[A, A]): MergeableEither[A] = new MergeableEither(x)

  /**
   * Projects an `Either` into a `Left`.
   *
   * This allows for-comprehensions over Either instances - for example {{{
   * for (s <- Left("flower").left) yield s.length // Left(6)
   * }}}
   *
   * Continuing the analogy with [[scala.Option]], a `LeftProjection` declares
   * that `Left` should be analogous to `Some` in some code.
   *
   * {{{
   * // using Option:
   * def interactWithDB(x: Query): Option[Result] =
   *   try {
   *     Some(getResultFromDatabase(x))
   *   } catch {
   *     case ex => None
   *   }
   *
   * // this will only be executed if interactWithDB returns a Some
   * val report =
   *   for (r <- interactWithDB(someQuery)) yield generateReport(r)
   * if (report.isDefined)
   *   send(report)
   * else
   *   log("report not generated, not sure why...")
   * }}}
   *
   * {{{
  * // using Either
   * def interactWithDB(x: Query): Either[Exception, Result] =
   *   try {
   *     Right(getResultFromDatabase(x))
   *   } catch {
   *     case ex => Left(ex)
   *   }
   *
   * // this will only be executed if interactWithDB returns a Right
   * val report =
   *   for (r <- interactWithDB(someQuery).right) yield generateReport(r)
   * if (report.isRight)
   *   send(report)
   * else
   *   log("report not generated, reason was " + report.left.get)
   * }}}
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
   * @version 1.0, 11/10/2008
   */
  final case class LeftProjection[+A, +B](e: Either[A, B]) {
    /**
     * Returns the value from this `Left` or throws `Predef.NoSuchElementException`
     * if this is a `Right`.
     *
     * {{{
     * Left(12).left.get // 12
     * Right(12).left.get // NoSuchElementException
     * }}}
     *
     * @throws Predef.NoSuchElementException if the projection is [[scala.Right]]
     */
    def get: A = e match {
      case Left(a) => a
      case Right(_) =>  throw new NoSuchElementException("Either.left.get on Right")
    }

    /**
     * Executes the given side-effecting function if this is a `Left`.
     *
     * {{{
     * Left(12).left.foreach(x => println(x))  // prints "12"
     * Right(12).left.foreach(x => println(x)) // doesn't print
     * }}}
     * @param f The side-effecting function to execute.
     */
    def foreach[U](f: A => U): Unit = e match {
      case Left(a) => f(a)
      case Right(_) => {}
    }

    /**
     * Returns the value from this `Left` or the given argument if this is a
     * `Right`.
     *
     * {{{
     * Left(12).left.getOrElse(17)  // 12
     * Right(12).left.getOrElse(17) // 17
     * }}}
     *
     */
    def getOrElse[AA >: A](or: => AA): AA = e match {
      case Left(a) => a
      case Right(_) => or
    }

    /**
     * Returns `true` if `Right` or returns the result of the application of
     * the given function to the `Left` value.
     *
     * {{{
     * Left(12).left.forall(_ > 10)  // true
     * Left(7).left.forall(_ > 10)   // false
     * Right(12).left.forall(_ > 10) // true
     * }}}
     *
     */
    def forall(f: A => Boolean): Boolean = e match {
      case Left(a) => f(a)
      case Right(_) => true
    }

    /**
     * Returns `false` if `Right` or returns the result of the application of
     * the given function to the `Left` value.
     *
     * {{{
     * Left(12).left.exists(_ > 10)  // true
     * Left(7).left.exists(_ > 10)   // false
     * Right(12).left.exists(_ > 10) // false
     * }}}
     *
     */
    def exists(f: A => Boolean): Boolean = e match {
      case Left(a) => f(a)
      case Right(_) => false
    }

    /**
     * Binds the given function across `Left`.
     *
     * {{{
     * Left(12).left.flatMap(x => Left("scala")) // Left("scala")
     * Right(12).left.flatMap(x => Left("scala") // Right(12)
     * }}}
     * @param f The function to bind across `Left`.
     */
    def flatMap[BB >: B, X](f: A => Either[X, BB]): Either[X, BB] = e match {
      case Left(a) => f(a)
      case Right(b) => Right(b)
    }

    /**
     * Maps the function argument through `Left`.
     *
     * {{{
     * Left(12).left.map(_ + 2) // Left(14)
     * Right[Int, Int](12).left.map(_ + 2) // Right(12)
     * }}}
     */
    def map[X](f: A => X): Either[X, B] = e match {
      case Left(a) => Left(f(a))
      case Right(b) => Right(b)
    }

    /**
     * Returns `None` if this is a `Right` or if the given predicate
     * `p` does not hold for the left value, otherwise, returns a `Left`.
     *
     * {{{
     * Left(12).left.filter(_ > 10)  // Some(Left(12))
     * Left(7).left.filter(_ > 10)   // None
     * Right(12).left.filter(_ > 10) // None
     * }}}
     */
    def filter[Y](p: A => Boolean): Option[Either[A, Y]] = e match {
      case Left(a) => if(p(a)) Some(Left(a)) else None
      case Right(b) => None
    }

    /**
     * Returns a `Seq` containing the `Left` value if it exists or an empty
     * `Seq` if this is a `Right`.
     *
     * {{{
     * Left(12).left.toSeq // Seq(12)
     * Right(12).left.toSeq // Seq()
     * }}}
     */
    def toSeq: Seq[A] = e match {
      case Left(a) => Seq(a)
      case Right(_) => Seq.empty
    }

    /**
     * Returns a `Some` containing the `Left` value if it exists or a
     * `None` if this is a `Right`.
     *
     * {{{
     * Left(12).left.toOption // Some(12)
     * Right(12).left.toOption // None
     * }}}
     */
    def toOption: Option[A] = e match {
      case Left(a) => Some(a)
      case Right(_) => None
    }
  }

  /**
   * Projects an `Either` into a `Right`.
   *
   * This allows for-comprehensions over Either instances - for example {{{
   * for (s <- Right("flower").right) yield s.length // Right(6)
   * }}}
   *
   * Continuing the analogy with [[scala.Option]], a `RightProjection` declares
   * that `Right` should be analogous to `Some` in some code.
   *
   * Analogous to `LeftProjection`, see example usage in its documentation above.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>, Workingmouse
   * @version 1.0, 11/10/2008
   */
  final case class RightProjection[+A, +B](e: Either[A, B]) {

    /**
     * Returns the value from this `Right` or throws
     * `Predef.NoSuchElementException` if this is a `Left`.
     *
     * {{{
     * Right(12).right.get // 12
     * Left(12).right.get // NoSuchElementException
     * }}}
     *
     * @throws Predef.NoSuchElementException if the projection is `Left`.
     */
    def get: B = e.get
    /**
     * Executes the given side-effecting function if this is a `Right`.
     *
     * {{{
     * Right(12).right.foreach(x => println(x)) // prints "12"
     * Left(12).right.foreach(x => println(x))  // doesn't print
     * }}}
     * @param f The side-effecting function to execute.
     */
    @inline def foreach[U](f: B => U): Unit = e.foreach(f)

    /**
     * Returns the value from this `Right` or the given argument if this is a
     * `Left`.
     *
     * {{{
     * Right(12).right.getOrElse(17) // 12
     * Left(12).right.getOrElse(17)  // 17
     * }}}
     */
    @inline def getOrElse[BB >: B](or: => BB): BB = e.getOrElse(or)

    /**
     * Returns `true` if `Left` or returns the result of the application of
     * the given function to the `Right` value.
     *
     * {{{
     * Right(12).right.forall(_ > 10) // true
     * Right(7).right.forall(_ > 10)  // false
     * Left(12).right.forall(_ > 10)  // true
     * }}}
     */
    @inline def forall(f: B => Boolean): Boolean = e.forall(f)

    /**
     * Returns `false` if `Left` or returns the result of the application of
     * the given function to the `Right` value.
     *
     * {{{
     * Right(12).right.exists(_ > 10)  // true
     * Right(7).right.exists(_ > 10)   // false
     * Left(12).right.exists(_ > 10)   // false
     * }}}
     */
    @inline def exists(f: B => Boolean): Boolean = e.exists(f)

    /**
     * Binds the given function across `Right`.
     *
     * @param f The function to bind across `Right`.
     */
    def flatMap[AA >: A, Y](f: B => Either[AA, Y]): Either[AA, Y] = e.flatMap(f)

    /**
     * The given function is applied if this is a `Right`.
     *
     * {{{
     * Right(12).right.map(x => "flower") // Result: Right("flower")
     * Left(12).right.map(x => "flower")  // Result: Left(12)
     * }}}
     */
    @inline def map[Y](f: B => Y): Either[A, Y] = e.map(f)

    /** Returns `None` if this is a `Left` or if the
     *  given predicate `p` does not hold for the right value,
     *  otherwise, returns a `Right`.
     *
     * {{{
     * Right(12).right.filter(_ > 10) // Some(Right(12))
     * Right(7).right.filter(_ > 10)  // None
     * Left(12).right.filter(_ > 10)  // None
     * }}}
     */
    def filter[X](p: B => Boolean): Option[Either[X, B]] = e match {
      case Left(_) => None
      case Right(b) => if(p(b)) Some(Right(b)) else None
    }

    /** Returns a `Seq` containing the `Right` value if
     *  it exists or an empty `Seq` if this is a `Left`.
     *
     * {{{
     * Right(12).right.toSeq // Seq(12)
     * Left(12).right.toSeq // Seq()
     * }}}
     */
    def toSeq: Seq[B] = e.toSeq

    /** Returns a `Some` containing the `Right` value
     *  if it exists or a `None` if this is a `Left`.
     *
     * {{{
     * Right(12).right.toOption // Some(12)
     * Left(12).right.toOption // None
     * }}}
     */
    def toOption: Option[B] = e.toOption
  }

  /** If the condition is satisfied, return the given `B` in `Right`,
   *  otherwise, return the given `A` in `Left`.
   *
   * {{{
   * val userInput: String = ...
   * Either.cond(
   *   userInput.forall(_.isDigit) && userInput.size == 10,
   *   PhoneNumber(userInput),
   *   "The input (%s) does not look like a phone number".format(userInput)
   * }}}
   */
  def cond[A, B](test: Boolean, right: => B, left: => A): Either[A, B] =
    if (test) Right(right) else Left(left)
}
