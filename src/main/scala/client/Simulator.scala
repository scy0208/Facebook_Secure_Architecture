package client

import util.Random
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.util.Timeout
//inform server finish
  
class Simulator(firstNamePool: List[String], lastNamePool: List[String], behaviorPool: List[String], birthdayPool: List[String], num: Int) extends Actor{
    var index0 = 0
    var index1 = 0
    var index2 = 0
    def receive = {
      case Initialize => {
        println("Enter Intialize, get server public key")
        for (i <- 1 to num){
          val client = context.actorOf(Props(new SingleClient(behaviorPool(Random.nextInt(behaviorPool.length)))), i.toString())
          client ! PreRegister(self)
        }
      }
      
      case InitializeDone =>{
        index0 += 1
        if (index0 == num){
          println("Got server public key")
          self ! Regi   
        }
      }
      
      case Regi => {  //let all client to register itself to server
        println("Enter Intialize")
        for (i <- 1 to num){
          val client = context.actorFor("akka://ClientSimulator/user/simu/" + i.toString())
          val name = firstNamePool(Random.nextInt(firstNamePool.length))+lastNamePool(Random.nextInt(lastNamePool.length))
          val bod = birthdayPool(Random.nextInt(birthdayPool.length))
          client ! Register(name, bod, self)
        }
      }
      
      case RegisterDone =>{
        index1 += 1
        //println("Receregist done ===" + index1)
        if (index1 == num){
          println("Finish Register *****************")
          val client = context.actorFor("akka://ClientSimulator/user/simu/" + 1.toString())
          client ! FinishAll(self)
        }
      }
      
      case Done => {  //send message to client to post before simulation
        for (i <- 1 to num){
          val client = context.actorFor("akka://ClientSimulator/user/simu/" + i.toString())
          client ! TestPost1
        }
      }
      
      case FinishIniPost => {
        index2 += 1
        if (index2 == num){
          println("************ Start to Simulate *************")
          Thread.sleep(100*num)
          for (i <- 1 to num){
            val client = context.actorFor("akka://ClientSimulator/user/simu/" + i.toString())
            //client ! Simu
            //client !
          }
        }
      }
      case Error(e) => {
        println(e)
      }
    }
  }