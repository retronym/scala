package scala.tools.nsc

import java.io.Closeable
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor, _}

import scala.collection.JavaConverters.{asScalaBufferConverter, bufferAsJavaListConverter}
import scala.reflect.internal.pickling.ByteCodecs
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.{ClassReader, ClassWriter, Opcodes}

object PickleExtractor {

  abstract class RootPath extends Closeable {
    def root: Path
  }

  def rootPath(path: Path, writable: Boolean): RootPath = {
    if (path.getFileName.toString.endsWith(".jar")) {
      import java.net.URI
      val zipFile = URI.create("jar:file:" + path.toUri.getPath)
      val env = new java.util.HashMap[String, String]()
      if (!Files.exists(path.getParent))
        Files.createDirectories(path.getParent)
      if (writable) {
        env.put("create", "true")
        if (Files.exists(path))
          Files.delete(path)
      }
      val zipfs = FileSystems.newFileSystem(zipFile, env)
      new RootPath {
        def root = zipfs.getRootDirectories.iterator().next()
        def close(): Unit = {
          zipfs.close()
        }
      }
    } else {
      new RootPath {
        override def root: Path = path
        override def close(): Unit = ()
      }
    }
  }
  def main(args: Array[String]): Unit = {
    args.toList match {
      case input :: output :: Nil =>
        process(Paths.get(input), Paths.get(output))
      case _ =>
    }
  }
  def process(input: Path, output: Path): Unit = {
    val inputPath = rootPath(input, writable = false)
    val outputPath = rootPath(output, writable = true)
    try {
      val root = inputPath.root
      Files.createDirectories(outputPath.root)
      val visitor = new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (dir != root) {
            val outputDir = outputPath.root.resolve(root.relativize(dir).toString)
            Files.createDirectories(outputDir)
          }
          FileVisitResult.CONTINUE
        }
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (file.getFileName.toString.endsWith(".class")) {
            stripClassFile(Files.readAllBytes(file)) match {
              case Class(out) =>
                Files.write(outputPath.root.resolve(root.relativize(file).toString), out)
              case Pickle(out) =>
                Files.write(outputPath.root.resolve(root.relativize(file).toString.replaceAll(".class$", ".sig")), out)
              case Skip =>
            }
          }
          FileVisitResult.CONTINUE
        }
      }
      Files.walkFileTree(root, visitor)
    } finally {
      inputPath.close()
      outputPath.close()
    }
  }

  def stripClassFile(classfile: Array[Byte]): OutputFile = {
    val input = new ClassNode()
    new ClassReader(classfile).accept(input, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES | ClassReader.SKIP_CODE)
    var output = new ClassNode()
    output.name = input.name
    output.access = input.access
    output.version = input.version

    var foundScalaSig = false

    def isScalaAnnotation(desc: String) = (desc == "Lscala/reflect/ScalaSignature;" || desc == "Lscala/reflect/ScalaLongSignature;") && {
      foundScalaSig = true

      true
    }

    var pickleData: Array[Byte] = null
    if (input.visibleAnnotations != null) {
      input.visibleAnnotations.asScala.foreach { node =>
        if (node.desc == "Lscala/reflect/ScalaSignature;") {
          val Array("bytes", data: String) = node.values.toArray()
          val bytes = data.getBytes(java.nio.charset.StandardCharsets.UTF_8)
          val len = ByteCodecs.decode(bytes)
          pickleData = bytes.take(len)
        } else if (node.desc == "Lscala/reflect/ScalaLongSignature;") {
          val Array("bytes", data: Array[String]) = node.values.toArray()
          val encoded = data flatMap (_.getBytes(java.nio.charset.StandardCharsets.UTF_8))
          val len = ByteCodecs.decode(encoded)
          pickleData = encoded.take(len)
        }
      }
      output.visibleAnnotations = input.visibleAnnotations.asScala.filter(node => isScalaAnnotation(node.desc) && {
        node;
        node.values
        true
      }).asJava
    }
    var foundScalaAttr = false
    if (input.attrs != null) {
      output.attrs = input.attrs.asScala.filter(attr => (attr.`type` == "Scala" || attr.`type` == "ScalaSig") && {
        foundScalaAttr = true;
        true
      }).asJava
    }
    val writer = new ClassWriter(Opcodes.ASM7_EXPERIMENTAL)
    val isScalaRaw = foundScalaAttr && !foundScalaSig
    if (isScalaRaw) Skip
    else {
      if (pickleData == null) {
        output = input
        output.accept(writer)
        Class(writer.toByteArray)
      } else {
        output.accept(writer)
        Pickle(pickleData)
      }
    }
  }

  sealed abstract class OutputFile

  case object Skip extends OutputFile

  case class Class(content: Array[Byte]) extends OutputFile

  case class Pickle(content: Array[Byte]) extends OutputFile

}
