package luna.projectmanager

import java.io.File
import java.util.UUID

import luna.projectmanager.model.{Config, Project}

import scala.io.Source
import scala.util.Try
import io.circe.yaml
import io.circe.generic.auto._

import scala.collection.immutable.HashMap


case class ProjectCacheItem(path: String, id: UUID, lastOpen: Option[Long])

class ProjectsRepository(settingsPath: File, projectsPath: File) {
  var projects: HashMap[UUID, Project] = readProjects

  def readProjects: HashMap[UUID, Project] = {
    val projects = listProjectsInDirectory
    val cache = readCache
    processProjectsWithCache(projects, cache)
  }

  def projectsFromCache(cache: List[ProjectCacheItem]): List[(UUID, Project)] = {
    cache.flatMap(cacheItem => {
      val path = new File(cacheItem.path)
      val config = Config.readFromPackage(path)
      config.map(cfg => (cacheItem.id, Project(path, cfg, cacheItem.lastOpen)))
    })
  }

  def listProjectsInDirectory: List[Project] = {
    val candidates = projectsPath.listFiles(_.isDirectory).toList
    candidates.flatMap(candidate => {
      val config = Config.readFromPackage(candidate)
      config.map(Project(candidate, _, None))
    })
  }

  def processProjectsWithCache(projects: List[Project], cache: List[ProjectCacheItem]): HashMap[UUID, Project] = {
    val cacheMap = cache.map(item => new File(item.path).getAbsolutePath -> item).toMap
    val fixedProjects = projects.map(project => {
      cacheMap
        .get(project.path.getAbsolutePath)
        .map(cacheItem =>
          (cacheItem.id, project.copy(lastOpen=cacheItem.lastOpen)))
        .getOrElse((UUID.randomUUID(), project))
    })
    HashMap(fixedProjects: _*)
  }

  def readCache: List[ProjectCacheItem] = {
    val cachePath = new File(settingsPath, "projects-cache.yaml")
    val source = Try(Source.fromFile(cachePath))
    val result = source
      .map(_.mkString)
      .flatMap(contents => {
        yaml.parser.parse(contents).flatMap(_.as[List[ProjectCacheItem]]).toTry
      })
      .getOrElse(Nil)
    source.foreach(_.close())
    result
  }
}
