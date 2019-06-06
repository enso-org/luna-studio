package luna.projectmanager

import java.io.File
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.server.ExceptionHandler
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives
import akka.stream.ActorMaterializer
import luna.projectmanager.api.{Project, ProjectJsonSupport}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


class Server(host: String, port: Int, repository: ProjectsRepository)(implicit val system: ActorSystem, implicit val executor: ExecutionContext, implicit val materializer: ActorMaterializer) extends Directives with ProjectJsonSupport {

  val exceptionHandler = ExceptionHandler {
    case DoesNotExistException(id) =>
      complete(HttpResponse(StatusCodes.NotFound, entity=s"Project $id does not exist."))
  }

  val route = ignoreTrailingSlash {
    handleExceptions(exceptionHandler) {
      pathPrefix("projects") {
        pathSingleSlash {
          get {
            extractUri { uri =>
              val response = repository.projects.toSeq.map { case (id, project) =>
                Project.fromModel(id, project, uri)
              }
              complete(response)
            }
          }
        } ~
        pathPrefix(Segment) { id =>
          val uid = Try(UUID.fromString(id)).getOrElse(throw DoesNotExistException(id))
          val project = repository.getById(uid)
          path("thumb") {
            get {
              if (project.hasThumb)
                getFromFile(project.thumbPath)
              else
                complete(HttpResponse(StatusCodes.NotFound, entity="Thumbnail does not exist"))
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

object Server extends App {

  override def main(args: Array[String]) {
    val host = "0.0.0.0"
    val port = 12345
    implicit val system: ActorSystem = ActorSystem("luna-studio")
    implicit val executor: ExecutionContext = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()


   // val bindingFuture = Http().bindAndHandle(route, host, port)
    val repo = new ProjectsRepository(new File("/Users/marcinkostrzewa/.luna/storage"), new File("/Users/marcinkostrzewa/luna/projects"))
    val server = new Server(host, port, repo)
    server.serve
  }
}
