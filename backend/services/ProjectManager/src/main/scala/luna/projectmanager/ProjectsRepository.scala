package luna.projectmanager

import java.io.File
import java.util.UUID

import akka.actor.Status.Success
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.StashBuffer
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

case class ProjectsStorageManager(
  localProjectsPath: File,
  tmpProjectsPath: File,
  tutorialsPath: File) {

  def persist(project: Project, newName: Option[String]): Project = {
    val pkg     = project.pkg
    val renamed = newName.map(pkg.rename).getOrElse(pkg)
    val root    = assignRootForName(localProjectsPath, renamed.name)
    val moved   = renamed.move(root)
    Project(Local, moved)
  }

  def assignRootForName(
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

  def readLocalProjects: ProjectsRepository =
    listProjectsInDirectory(Local, localProjectsPath)

  def readTutorials: ProjectsRepository =
    listProjectsInDirectory(Tutorial, tutorialsPath)

  def listProjectsInDirectory(
    kind: ProjectType,
    dir: File
  ): ProjectsRepository = {
    val candidates = dir.listFiles(_.isDirectory).toList
    val projects   = candidates.map(Package.getOrCreate).map(Project(kind, _))
    ProjectsRepository(HashMap(projects.map(UUID.randomUUID() -> _): _*))

  }

  def createTemporary(name: String): Project = {
    val root = assignRootForName(tmpProjectsPath, name)
    val pkg  = Package.create(root, name)
    Project(Temporary, pkg)
  }
}

case class ProjectsRepository(projects: HashMap[UUID, Project]) {

  def getById(id: UUID): Option[Project] = {
    projects.get(id)
  }

  def insert(project: Project): (UUID, ProjectsRepository) = {
    val id      = UUID.randomUUID()
    val newRepo = copy(projects = projects + (id -> project))
    (id, newRepo)
  }

}

sealed trait ProjectsCommand extends InternalProjectsCommand
sealed trait InternalProjectsCommand

case class ListTutorialsRequest(replyTo: ActorRef[ListProjectsResponse])
    extends ProjectsCommand
case class ListProjectsRequest(replyTo: ActorRef[ListProjectsResponse])
    extends ProjectsCommand
case class ListProjectsResponse(projects: HashMap[UUID, Project])

case class GetProjectById(id: UUID, replyTo: ActorRef[GetProjectResponse])
    extends ProjectsCommand
case class GetProjectResponse(project: Option[Project])

case class CreateTemporary(
  name: String,
  replyTo: ActorRef[CreateTemporaryResponse])
    extends ProjectsCommand
case class CreateTemporaryResponse(id: UUID, project: Project)

case object TutorialsReady extends InternalProjectsCommand

object ProjectsService {

  def behavior(
    storageManager: ProjectsStorageManager,
    tutorialsDownloader: TutorialsDownloader
  ): Behavior[InternalProjectsCommand] = Behaviors.setup { context =>
    val buffer = StashBuffer[InternalProjectsCommand](capacity = 100)

    def handle(
      localRepo: ProjectsRepository,
      tutorialsRepo: Option[ProjectsRepository]
    ): Behavior[InternalProjectsCommand] = Behaviors.receiveMessage {
      case ListProjectsRequest(replyTo) =>
        replyTo ! ListProjectsResponse(localRepo.projects)
        Behaviors.same
      case msg: ListTutorialsRequest =>
        tutorialsRepo match {
          case Some(repo) => msg.replyTo ! ListProjectsResponse(repo.projects)
          case None       => buffer.stash(msg)
        }
        Behaviors.same
      case GetProjectById(id, replyTo) =>
        val project =
          localRepo.getById(id).orElse(tutorialsRepo.flatMap(_.getById(id)))
        replyTo ! GetProjectResponse(project)
        Behaviors.same
      case TutorialsReady =>
        val newTutorialsRepo = storageManager.readTutorials
        buffer.unstashAll(context, handle(localRepo, Some(newTutorialsRepo)))
      case msg: CreateTemporary =>
        val project =
          storageManager.createTemporary(msg.name)
        val (projectId, newProjectsRepo) = localRepo.insert(project)
        msg.replyTo ! CreateTemporaryResponse(projectId, project)
        handle(newProjectsRepo, tutorialsRepo)
    }

    context.pipeToSelf(tutorialsDownloader.run())(_ => TutorialsReady)

    handle(storageManager.readLocalProjects, None)
  }
}
