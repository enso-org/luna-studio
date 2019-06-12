package luna.projectmanager.api

import java.util.UUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path
import org.enso.pkg.Package
import spray.json.DefaultJsonProtocol

case class Project(
    id: String,
    name: String,
    path: String,
    thumb: Option[String],
    lastOpenTime: Option[Long],
    persisted: Boolean)

object Project {
  def fromModel(
      id: UUID,
      project: Package,
      baseUri: Uri,
      persisted: Boolean
    ): Project = {
    val thumbUri =
      if (project.hasThumb) Some(baseUri.withPath(thumbPath(id))) else None
    Project(
      id.toString,
      project.name,
      project.root.getAbsolutePath,
      thumbUri.map(_.toString()),
      None,
      persisted
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
