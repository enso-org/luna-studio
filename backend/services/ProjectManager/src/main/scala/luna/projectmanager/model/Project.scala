package luna.projectmanager.model

import java.io.File

import io.circe.yaml
import io.circe.generic.auto._

import scala.io.Source
import scala.util.Try

case class Config(author: String, maintainer: String, name: String)

object Config {
  def fromYaml(yamlString: String): Option[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toOption
  }

  def readYaml(path: File): Option[Config] = {
    val source = Try(Source.fromFile(path))
    val result = source.map(_.mkString).toOption.flatMap(fromYaml)
    source.foreach(_.close())
    result
  }

  def readYaml(path: String): Option[Config] = readYaml(new File(path))

  def fileName: String = "package.yaml"

  def readFromPackage(file: File): Option[Config] = {
    readYaml(new File(file, fileName))
  }
}

case class Project(path: File, config: Config, lastOpen: Option[Long]) {
  def name: String = config.name
  def thumbFilename = "thumb.png"
  def hasThumb: Boolean = new File(path, thumbFilename).exists()
}
