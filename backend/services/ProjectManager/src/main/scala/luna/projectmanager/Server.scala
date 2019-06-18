package luna.projectmanager

import java.io.File
import java.util.UUID

import akka.actor.ActorSystem
import akka.actor.Scheduler
import akka.actor.typed.ActorRef
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.ExceptionHandler
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import luna.projectmanager.api.ProjectJsonSupport
import luna.projectmanager.api.{Project => ApiProject}
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.concurrent.duration._
import akka.actor.typed.scaladsl.adapter._

case class Server(
  host: String,
  port: Int,
  repository: ActorRef[ProjectsCommand]
)(implicit val system: ActorSystem,
  implicit val executor: ExecutionContext,
  implicit val materializer: ActorMaterializer)
    extends Directives
    with ProjectJsonSupport {

  val exceptionHandler = ExceptionHandler {
    case DoesNotExistException(id) =>
      complete(
        HttpResponse(
          StatusCodes.NotFound,
          entity = s"Project $id does not exist."
        )
      )
  }

  implicit val timeout: Timeout     = 10.seconds
  implicit val scheduler: Scheduler = system.scheduler

  def withProject(id: String)(route: Project => Route): Route = {
    val uid =
      Try(UUID.fromString(id)).getOrElse(throw DoesNotExistException(id))
    val projectFuture = repository.ask(ref => GetProjectById(uid, ref))
    onComplete(projectFuture) {
      case Success(GetProjectResponse(Some(project))) => route(project)
      case _ =>
        throw DoesNotExistException(id)
    }
  }

  def withSuccess[T](
    fut: Future[T],
    errorResponse: Option[HttpResponse] = None
  )(successHandler: T => Route
  ): Route = {
    onComplete(fut) {
      case Success(r) => successHandler(r)
      case Failure(_) =>
        val response =
          errorResponse.getOrElse(HttpResponse(StatusCodes.InternalServerError))
        complete(response)
    }
  }

  val route: Route = ignoreTrailingSlash {
    handleExceptions(exceptionHandler) {
      pathPrefix("projects") {
        (pathSingleSlash & extractUri & get) { uri =>
          val projectsFuture = repository.ask(ListProjectsRequest)
          withSuccess(projectsFuture) { projectsResponse =>
            val response = projectsResponse.projects.toSeq.map {
              case (id, project) => ApiProject.fromModel(id, project, uri)
            }
            complete(response)
          }
        } ~ (pathSingleSlash & extractUri & post) { uri =>
          val projectFuture = repository.ask(CreateTemporary("NewProject", _))
          withSuccess(projectFuture) {
            case CreateTemporaryResponse(id, project) =>
              complete(ApiProject.fromModel(id, project, uri))
          }
        } ~ (pathPrefix(Segment) & path("thumb") & get) { id =>
          withProject(id) { project =>
            if (project.pkg.hasThumb) getFromFile(project.pkg.thumbFile)
            else
              complete(
                HttpResponse(
                  StatusCodes.NotFound,
                  entity = "Thumbnail does not exist"
                )
              )
          }
        }
      } ~
      pathPrefix("tutorials") {
        pathSingleSlash {
          get {
            extractUri { uri =>
              val projectsFuture = repository.ask(ListTutorialsRequest)
              withSuccess(projectsFuture) { projectsResponse =>
                val response = projectsResponse.projects.toSeq.map {
                  case (id, project) => ApiProject.fromModel(id, project, uri)
                }
                complete(response)
              }
            }
          }
        }
      }
    }
  }

  def serve: Future[Http.ServerBinding] = {
    Http().bindAndHandle(route, host, port)
  }
}

object Server {

  def main(args: Array[String]) {
    val host = "0.0.0.0"
    val port = 50505

    implicit val system: ActorSystem             = ActorSystem("luna-studio")
    implicit val executor: ExecutionContext      = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val rootProjectsPath   = new File("/Users/marcinkostrzewa/luna/")
    val localProjectsPath  = new File(rootProjectsPath, "projects")
    val tmpProjectsPath    = new File(rootProjectsPath, "tmp")
    val tutorialsPath      = new File(rootProjectsPath, "tutorials")
    val tutorialsCachePath = new File(rootProjectsPath, ".tutorials-cache")

    val tutorialsDownloader =
      TutorialsDownloader(tutorialsPath, tutorialsCachePath)
    val storageManager = ProjectsStorageManager(
      localProjectsPath,
      tmpProjectsPath,
      tutorialsPath
    )

    val repoActor = system.spawn(
      ProjectsService.behavior(storageManager, tutorialsDownloader),
      "projects-repo"
    )
    val server = new Server(host, port, repoActor)
    server.serve
  }
}
