package luna.projectmanager

import java.io.File
import java.util.UUID

import org.enso.pkg.Package

import scala.collection.immutable.HashMap

case class DoesNotExistException(id: String) extends Exception

sealed trait ProjectType {
  def isPersistent: Boolean
}
case object Local extends ProjectType {
  override def isPersistent: Boolean = true
}
case object Tutorial extends ProjectType {
  override def isPersistent: Boolean = false
}
case object Temporary extends ProjectType {
  override def isPersistent: Boolean = false
}

case class Project(kind: ProjectType, pkg: Package) {
  def isPersistent: Boolean = kind.isPersistent
}

class ProjectsRepository(rootProjectsPath: File) {
  val projectsPath = new File(rootProjectsPath, "projects")
  val tmpProjectsPath = new File(rootProjectsPath, "tmp")
  val tutorialsPath = new File(rootProjectsPath, "tutorials")

  var projects: HashMap[UUID, Project] = readProjects(projectsPath)
  def getById(id: UUID): Project = {
    projects.getOrElse(id, throw DoesNotExistException(id.toString))
  }

  def persist(projectId: UUID, newName: Option[String] = None): Project = {
    val project = getById(projectId)
    val pkg = project.pkg
    val renamed = newName.map(pkg.rename).getOrElse(pkg)
    val root = assignRootForName(projectsPath, renamed.name)
    val moved = renamed.move(root)
    val newProject = Project(Local, moved)
    projects += projectId -> newProject
    newProject
  }

  def createTemporary(name: String): UUID = {
    val root = assignRootForName(tmpProjectsPath, name)
    val pkg = Package.create(root, name)
    val id = UUID.randomUUID()
    projects += id -> Project(Temporary, pkg)
    id
  }

  private def assignRootForName(
      rootDir: File,
      name: String,
      idx: Option[Int] = None
    ): File = {
    val nameToTry = s"$name.${idx.getOrElse("")}"
    val rootToTry = new File(rootDir, nameToTry)

    if (rootToTry.exists()) {
      assignRootForName(rootDir, name, Some(idx.map(_ + 1).getOrElse(0)))
    }
    else rootToTry
  }

  private def readProjects(dir: File): HashMap[UUID, Project] = {
    val projects = listProjectsInDirectory(dir)
    HashMap(
      projects.map(UUID.randomUUID() -> Project(Local, _)): _*
    )
  }

  private def listProjectsInDirectory(dir: File): List[Package] = {
    val candidates = dir.listFiles(_.isDirectory).toList
    candidates.map(Package.getOrCreate)
  }

}
