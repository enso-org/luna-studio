package luna.projectmanager

import java.io.File
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.ExceptionHandler
import akka.stream.ActorMaterializer
import luna.projectmanager.api.ProjectJsonSupport
import luna.projectmanager.api.{Project => ApiProject}

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

class Server(
    host: String,
    port: Int,
    repository: ProjectsRepository
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

  val route = ignoreTrailingSlash {
    handleExceptions(exceptionHandler) {
      pathPrefix("projects") {
        pathSingleSlash {
          get {
            extractUri { uri =>
              val response = repository.projects.toSeq.map {
                case (id, project) =>
                  ApiProject.fromModel(
                    id,
                    project.pkg,
                    uri,
                    project.isPersistent
                  )
              }
              complete(response)
            }
          }
        } ~ pathPrefix(Segment) { id =>
          val uid =
            Try(UUID.fromString(id)).getOrElse(throw DoesNotExistException(id))
          val project = repository.getById(uid)
          path("thumb") {
            get {
              if (project.pkg.hasThumb)
                getFromFile(project.pkg.thumbFile)
              else
                complete(
                  HttpResponse(
                    StatusCodes.NotFound,
                    entity = "Thumbnail does not exist"
                  )
                )
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
    implicit val system: ActorSystem = ActorSystem("luna-studio")
    implicit val executor: ExecutionContext = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val downloader = new TutorialsDownloader(
      new File("/Users/marcinkostrzewa/luna/tutorials"),
      new File("/Users/marcinkostrzewa/luna/.tutorials-cache")
    )
    val tuts = downloader.getAvailable
    val downfut = tuts.flatMap { tutorials =>
      Future
        .sequence(
          tutorials.map(
            tut =>
              downloader
                .downloadZip(tut)
                .map(
                  downloader.unzip
                )
          )
        )
    }
//    val files = Await.result(downfut, 30 seconds)
//    val files = List(
//      "CryptoCurrencyPlayground.zip",
//      "USElectionPlayground.zip",
//      "TramsPlayground.zip",
//      "TechnicalAnalysis.zip"
//    ).map(new File(downloader.workDir, _))
//    files.map(downloader.unzip)

    //    new TutorialsDownloader(new File("/Users/marcinkostrzewa/luna/tutorials")).cloneTutorials
    val repo = new ProjectsRepository(
      new File("/Users/marcinkostrzewa/luna/")
    )

    // val bindingFuture = Http().bindAndHandle(route, host, port)
//    val server = new Server(host, port, repo)
//    server.serve
  }
}
