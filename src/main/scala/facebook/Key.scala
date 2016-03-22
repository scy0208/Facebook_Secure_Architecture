package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class Key extends Actor{
  println("RSA Initialize ...")
  var cache = new HashMap[Int, String]()
  println("HashMap : " + cache.isEmpty)
  
  def receive = {
    case StoreKey(id, key) =>
      cache += (id -> key)
      cache.foreach(_ => println(" id : " + id  + " key: " + key)) 
    case getKey(id) =>
      val node = cache.get(id)
      node match{
        case Some(s) => sender ! s
        case _ => 
      }
  }
}