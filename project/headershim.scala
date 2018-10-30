package de.heikoseeberger.sbtheader

import sbt._
import sbt.Keys._
import sbt.plugins.JvmPlugin

object HeaderPlugin extends AutoPlugin {

  final object autoImport {
    class License
    object HeaderLicense {
      case class Custom(s: String) extends License
    }
    val headerLicense: SettingKey[Option[License]] = settingKey[Option[License]]("header License")

    val headerSources = taskKey[scala.collection.Seq[File]]("Sources which need headers checked or created.")

    val headerResources = taskKey[scala.collection.Seq[File]]("Resources which need headers checked or created.")

    def headerSettings(configurations: Configuration*): Seq[Setting[_]] =
      configurations.foldLeft(List.empty[Setting[_]]) { _ ++ inConfig(_)(toBeScopedSettings) }
  }

  import autoImport._

  override def trigger = allRequirements

  override def requires = JvmPlugin

  override def projectSettings = headerSettings(Compile, Test)

  private def toBeScopedSettings = Vector(headerSources := Nil, headerResources := Nil)

}
