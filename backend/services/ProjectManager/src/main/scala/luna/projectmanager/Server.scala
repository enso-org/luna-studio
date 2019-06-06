package luna.projectmanager

import java.io.File

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives
import akka.stream.ActorMaterializer

import scala.concurrent.{ExecutionContext, Future}

class Server(host: String, port: Int, repository: ProjectsRepository)(implicit val system: ActorSystem, implicit val executor: ExecutionContext, implicit val materializer: ActorMaterializer) extends Directives with JsonSupport {
  def serve: Future[Http.ServerBinding] = {
    val route = get {
      pathSingleSlash {
        complete("hello world!")
      }
    }
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
