package luna.projectmanager.api

import java.util.UUID

import akka.http.scaladsl.model.Uri
import luna.projectmanager.model

case class Project(id: UUID, name: String, path: String, thumb: Option[Uri], lastOpenTime: Option[Long])

object Project {
  def fromModel(id: UUID, project: model.Project, projectUri: Uri): Project = {
    val thumbUri = if (project.hasThumb) Some(projectUri.withPath(projectUri.path / project.thumbFilename)) else None
    Project(id, project.name, project.path.getAbsolutePath, thumbUri, None)
  }
}