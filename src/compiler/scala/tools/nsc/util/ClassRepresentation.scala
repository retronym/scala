/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.util

import scala.tools.nsc.io.AbstractFile

/**
 * Represents classes which can be loaded with a ClassfileLoader and/or SourcefileLoader.
 */
trait ClassRepresentation {
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]

  def name: String
}
