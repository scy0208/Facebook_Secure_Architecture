package client

import util.Random
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.util.Timeout

object ClientSimu {
  def main(args: Array[String]) {
    val system = ActorSystem("ClientSimulator")
    val firstNamePool = List("Alex", "Bob", "James", "Jack", "Filip" , "Terry", "Daniel", "Andrew")
    val lastNamePool = List("Benson", "Will", "Carl", "Smith", "Larson", "Antoni", "Miller", "George")
    val behaviorPool = List("Normal", "Crazy_Poster", "Crazy_Getter", "Slacker")
    val birthdayPool = List("19911212", "19911113", "19660129", "19671116", "19491001")
  
    val num = 1
    val simulator = system.actorOf(Props(classOf[Simulator], firstNamePool, lastNamePool, behaviorPool, birthdayPool, num), "simu")
    simulator ! Initialize
    // lasting some time, system stop simulate
   
    //system.stop(simulator)
  }

}