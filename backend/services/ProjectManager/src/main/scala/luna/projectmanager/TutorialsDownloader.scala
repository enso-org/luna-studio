package luna.projectmanager

import java.io.File
import java.io.FileOutputStream
import java.time.Instant
import java.util.zip.ZipFile

import akka.actor.ActorSystem
import akka.event.LogSource
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.FileIO
import org.apache.commons.io.IOUtils
import spray.json.DefaultJsonProtocol
import spray.json.JsonParser

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

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

case class HttpHelper(
    implicit val executor: ExecutionContext,
    implicit val system: ActorSystem,
    implicit val materializer: ActorMaterializer) {

  def performWithRedirects(request: HttpRequest): Future[HttpResponse] =
    Http().singleRequest(request).flatMap(followRedirects)

  def followRedirects(response: HttpResponse): Future[HttpResponse] =
    response.status match {
      case StatusCodes.Found =>
        response.entity.discardBytes()
        val redirectUri = response.header[Location].get.uri
        val newRequest = HttpRequest(uri = redirectUri)
        performWithRedirects(newRequest)
      case _ => Future(response)
    }
}

case class TutorialsDownloader(
    tutorialsDir: File,
    cacheDir: File
  )(implicit val system: ActorSystem,
    implicit val executor: ExecutionContext,
    implicit val materializer: ActorMaterializer)
    extends GithubJsonProtocol {
  val packagesGithubOrg = "luna-packages"
  val packagesGithubUrl = s"https://api.github.com/orgs/$packagesGithubOrg"
  val packagesGithubRepos = s"$packagesGithubUrl/repos"
  implicit val logSource: LogSource[TutorialsDownloader] = t =>
    "TutorialsDownloader"
  val logging = Logging(system, this)

  if (!tutorialsDir.exists) tutorialsDir.mkdirs()
  if (!cacheDir.exists) cacheDir.mkdirs()

  def downloadUrlFor(tutorial: GithubTutorial): String =
    s"https://github.com/$packagesGithubOrg/${tutorial.name}/archive/master.zip"

  def zipFileFor(tutorial: GithubTutorial): File =
    new File(cacheDir, s"${tutorial.name}.zip")

  def getAvailable: Future[List[GithubTutorial]] =
    Http()
      .singleRequest(HttpRequest(uri = packagesGithubRepos))
      .flatMap(resp => Unmarshal(resp.entity).to[String])
      .map(JsonParser(_).convertTo[List[GithubTutorial]])

  def downloadZip(ref: GithubTutorial): Future[File] = {
    val request = HttpRequest(uri = downloadUrlFor(ref))
    val response = HttpHelper().performWithRedirects(request)
    val target = zipFileFor(ref)
    response
      .flatMap(_.entity.dataBytes.runWith(FileIO.toPath(target.toPath)))
      .map(_ => target)
  }

  def unzip(source: File): Unit = {
    logging.info(s"Unzipping $source\n")
    val zipFile = new ZipFile(source)
    Try {
      zipFile.entries.asScala.foreach { entry =>
        logging.info(s"Extracting ${entry.getName}\n")
        val target = new File(tutorialsDir, entry.getName)
        if (entry.isDirectory) target.mkdirs()
        else {
          target.getParentFile.mkdirs()
          val in = zipFile.getInputStream(entry)
          val out = new FileOutputStream(target)
          Try(IOUtils.copy(in, out))
          out.close()
        }
      }
    }
    zipFile.close()
  }

}
