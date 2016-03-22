package facebook

import akka.actor.{ Actor, ActorRef, Props}
import spray.routing.HttpService
import spray.http.MediaTypes._
import spray.httpx.Json4sSupport
import org.json4s.DefaultFormats
import rsa._


class Server extends Actor with Service {
  implicit def actorRefFactory = context
  def receive = runRoute(rout)
}

// service module
trait Service extends HttpService with Json4sSupport {
  val mmu = actorRefFactory.actorOf(Props[MMU], "Memory")
  val AES = actorRefFactory.actorOf(Props[Cache], "Cache")

  //generate server P-, P+
  val rsa = new RSA(2048)
  val ServerPublicKey = rsa.publicKey
  val ServerPrivateKey = rsa.privateKey
  val modulus = rsa.modulus

  val json4sFormats = DefaultFormats
  val rout = {
    get {
      path(Segments) { segments =>
        validate(segments.exists(_.toString.matches("[0-9]*")), "unmatched path"){
          parameterSeq{ params =>
            val para = params
            val path = segments
            val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
            ctx => op ! GetRequest(path, para, ctx, mmu, AES, rsa)
          }
        }~
          validate(segments.exists(_.toString.matches("registerdone")), "unmatched path"){
            parameterSeq{ params =>
              val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
              ctx => op ! RegisterDone(mmu, ctx)
            }
          }~
          validate(segments.exists(_.toString.matches("preregister")), "unmatched path"){
            parameterSeq{ params =>
              val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
              //create empty node in MMU, then back id, RSA public key in server
              val para = params
              ctx => op ! CreateEmptyUser(ctx, mmu, AES, para, rsa)
            }
          }
      }
    }~
      post{
        path(Segments) { segments =>
          validate(segments.exists(_.toString.matches("page")), "unmatched path"){
            parameterSeq{ params =>
              val para = params
              val path = segments

              println("PAGE == para : " + para + "path : " + path)

              val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
              ctx => op ! CreateUser(path, para, ctx, mmu, AES, rsa)
            }
          }~
            validate(segments.exists(_.toString.matches("[0-9]*")), "unmatched path"){
              parameterSeq{ params =>
                val para = params
                val path = segments
                //println("HERE == para : " + para + "path : " + path)

                val op: ActorRef = actorRefFactory.actorOf(Props[Operator])
                ctx => op ! PostRequest(path, para, ctx, mmu, AES, rsa)
              }
            }
        }
      }
  }
}