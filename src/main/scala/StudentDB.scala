import java.io.{FileInputStream, FileOutputStream}
import java.io.{ObjectInputStream, ObjectOutputStream}
import scala.io.StdIn.{readLine}

import javax.swing.text.Segment

import scala.collection.mutable.ArrayBuffer

import io.circe.syntax._
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer




object StudentDB {
  implicit val system = ActorSystem("db")
  implicit val executionContext = system.dispatcher

  private val endpointName = "students"
  private val endpointSave = "save"
  private val endpointLoad = "load"
  private val endpointAction = "action"
  private val saveloc = "db"

  private var students = new ArrayBuffer[Student]

  def getRoutes: Route =
    get {
      concat(
        pathPrefix(endpointName / IntNumber) { id =>
          students.find(_.id == id) match {
            case Some(student) => complete(student.asJson)
            case None          => complete(StatusCodes.NotFound)
          }
        },
        path(endpointName) {
          complete(students.asJson)
        },
        path(endpointSave){
          val os = new ObjectOutputStream(new FileOutputStream(saveloc))
          os.writeObject(students)
          os.close()
          complete(StatusCodes.Success)
        },
        path(endpointLoad){
          val is = new ObjectInputStream(new FileInputStream(saveloc))
          val obj = is.readObject.asInstanceOf[ArrayBuffer[Student]]
          students = obj
          is.close()
          complete(StatusCodes.Success)
        }
      )
    }

  def postRoutes: Route =
    post {
      path(endpointName) {
        entity(as[Student]) { student =>
          students.indexWhere(_.id == student.id) match {
            case -1 =>
              students.append(student)
              complete(StatusCodes.OK)
            case _ => complete(StatusCodes.BadRequest)
          }
        }
      }
    }

  def deleteRoutes: Route =
    delete {
      pathPrefix(endpointName / IntNumber) { id =>
        students.find(_.id == id) match {
          case Some(student) =>
            students -= student
            complete(StatusCodes.OK)
          case None => complete(StatusCodes.NotFound)
        }
      }
    }

  def putRoutes: Route =
    put {
      pathPrefix(endpointName / IntNumber) { id =>
        entity(as[Student]) { student =>
          students.indexWhere(_.id == id) match {
            case -1 => complete(StatusCodes.NotFound)
            case index: Int  =>
              students(index) = student
              complete(StatusCodes.OK)
          }
        }
      }
    }

  def main(args: Array[String]): Unit = {
    val route = concat(getRoutes, postRoutes, deleteRoutes, putRoutes)

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }

}
