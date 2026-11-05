package io.github.retronym.sharing

import scala.annotation.StaticAnnotation

/** A field that readers may race on, by design: any writer stores the same value, or an equal one, so a lost or
 *  repeated write costs only work. As Error Prone's `@LazyInit`.
 */
final class benignRace(why: String) extends StaticAnnotation
