package luna.projectmanager

import java.io.File
import java.time.format.DateTimeFormatter
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import io.circe.Json
import org.eclipse.jgit.api.Git
import org.enso.pkg.Package
import spray.json.DefaultJsonProtocol
import spray.json.JsArray
import spray.json.JsObject
import spray.json.JsValue
import spray.json.JsonParser

import scala.collection.immutable.HashMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

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

case class GithubTutorial(
    name: String,
    cloneUrl: String,
    lastPushString: String) {
  val lastPush: Long = Instant.parse(lastPushString).getEpochSecond
}

trait GithubJsonProtocol extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val tutorialFormat =
    jsonFormat(GithubTutorial, "name", "clone_url", "pushed_at")
}

class TutorialsDownloader(
    workDir: File
  )(implicit val system: ActorSystem,
    implicit val executor: ExecutionContext,
    implicit val materializer: ActorMaterializer)
    extends GithubJsonProtocol {
  val packagesGithubOrg = "luna-packages"
  val packagesGithubUrl = s"https://api.github.com/orgs/$packagesGithubOrg"
  val packagesGithubRepos = s"$packagesGithubUrl/repos"

  def getTutorialsList: List[GithubTutorial] = {
    val future = Http()
      .singleRequest(HttpRequest(uri = packagesGithubRepos))
      .flatMap(resp => Unmarshal(resp.entity).to[String])
      .map(JsonParser(_).convertTo[List[GithubTutorial]])
    Await.result(future, 10 seconds)
  }

  def cloneTutorial(ref: GithubTutorial) = {
    Git
      .cloneRepository()
      .setURI(ref.cloneUrl)
      .setDirectory(new File(workDir, ref.name))
      .call()
      .close()
  }

  def cloneTutorials: Unit = getTutorialsList.foreach(cloneTutorial)
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
