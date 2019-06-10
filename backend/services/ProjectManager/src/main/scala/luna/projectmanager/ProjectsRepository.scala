package luna.projectmanager

import java.io.File
import java.util.UUID
import org.enso.pkg.Package
import scala.collection.immutable.HashMap

case class DoesNotExistException(id: String) extends Exception

class ProjectsRepository(rootProjectsPath: File) {
  val projectsPath = new File(rootProjectsPath, "projects")
  val tmpProjectsPath = new File(rootProjectsPath, "tmp")
  val tutorialsPath = new File(rootProjectsPath, "tutorials")

  var projects: HashMap[UUID, Package] = readProjects(projectsPath)

  def getById(id: UUID): Package = synchronized {
    projects.getOrElse(id, throw DoesNotExistException(id.toString))
  }

  def isPersistent(pkg: Package): Boolean = {
    pkg.root.getAbsolutePath.startsWith(projectsPath.getAbsolutePath)
  }

  def persist(pkg: Package): Unit = {}

  def createTemporary(name: String): Package = {
    val root = assignRootForName(tmpProjectsPath, name)
    Package.create(root, name)
  }

  private def assignRootForName(
      rootDir: File,
      name: String,
      idx: Option[Int] = None
    ): File = {
    val nameToTry = s"$name${idx.getOrElse("")}"
    val rootToTry = new File(rootDir, nameToTry)

    if (rootToTry.exists()) {
      assignRootForName(rootDir, name, Some(idx.map(_ + 1).getOrElse(0)))
    }
    else rootToTry
  }

  private def readProjects(dir: File): HashMap[UUID, Package] = {
    val projects = listProjectsInDirectory(dir)
    HashMap(projects.map(UUID.randomUUID() -> _): _*)
  }

  private def listProjectsInDirectory(dir: File): List[Package] = {
    val candidates = dir.listFiles(_.isDirectory).toList
    candidates.map(Package.getOrCreate)
  }

}
