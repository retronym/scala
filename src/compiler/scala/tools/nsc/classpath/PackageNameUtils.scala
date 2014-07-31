/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.classpath.FlatClassPath.RootPackage

/**
 * Common methods related to package names represented as String
 */
object PackageNameUtils {

	/**
	 * Returns last subpackage name:
	 * Example:
	 * baz for foo.bar.baz
	 * foo for foo
	 */
	def lastSubpackageNameForPackage(packageName: String): String = {
		val lastDotIndex = packageName.lastIndexOf('.')
		packageName.substring(lastDotIndex + 1)
	}

	/**
	 * @param fullClassName full class name with package
	 * @return (package, simple class name)
	 */
	def separatePkgAndClassNames(fullClassName: String): (String, String) = {
		val lastDotIndex = fullClassName.lastIndexOf('.')
		if (lastDotIndex == -1)
			(RootPackage, fullClassName)
		else
			(fullClassName.substring(0, lastDotIndex), fullClassName.substring(lastDotIndex + 1))
	}
}
