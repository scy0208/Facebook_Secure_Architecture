package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class Cache extends Actor{
  println("AES Initialize ...")
  var cache = new HashMap[String, (String, BigInt, BigInt)]()
  println("Cache : " + cache.isEmpty)
  
  def receive = {
    case Store(id, access, key, module) =>
      cache += (access -> (id,key,module))
      cache.foreach(_ => println("In cache ++ AC : " + access + " id : " + id  + " key: " + key + " module: "+ module))
    case getId(access) =>
      val node = cache.get(access)
      node match{
        case Some(s) => sender ! s._1
        case _ => 
      }
    case getAESKey(access) =>
      val node = cache.get(access)
      node match{
        case Some(s) => sender ! s._2
        case _ => 
      }
      
    case getAll(access) => 
      val node = cache.get(access)
      node match{
        case Some(s) => sender ! s
        case _ => 
      }
  }
}