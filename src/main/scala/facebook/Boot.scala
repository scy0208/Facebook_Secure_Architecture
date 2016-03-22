package facebook

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import spray.can.Http

object Boot extends App {
  implicit val system = ActorSystem("FB")
  val service = system.actorOf(Props[Server], "FB-Server")
  IO(Http) ! Http.Bind(service, interface = "127.0.0.1", port = 8080)
  
}