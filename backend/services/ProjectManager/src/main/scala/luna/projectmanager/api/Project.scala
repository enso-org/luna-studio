package luna.projectmanager.api

import java.util.UUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path
import luna.projectmanager.{Project => Model}
import spray.json.DefaultJsonProtocol

case class Project(
  id: String,
  name: String,
  path: String,
  thumb: Option[String],
  lastOpenTime: Option[Long],
  persisted: Boolean)

object Project {

  def fromModel(id: UUID, project: Model, baseUri: Uri): Project = {
    val thumbUri =
      if (project.pkg.hasThumb) Some(baseUri.withPath(thumbPath(id))) else None
    Project(
      id.toString,
      project.pkg.name,
      project.pkg.root.getAbsolutePath,
      thumbUri.map(_.toString()),
      None,
      project.isPersistent
    )
  }

  def projectPath(id: UUID): Path = {
    Path.Empty / "projects" / id.toString
  }

  def thumbPath(id: UUID): Path = {
    projectPath(id) / "thumb"
  }
}

trait ProjectJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val projectFormat = jsonFormat6(Project.apply)
}
