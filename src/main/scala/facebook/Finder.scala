package facebook

import akka.actor.Actor.Receive

import scala.collection.mutable
import akka.actor._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import scala.concurrent._
import akka.util.Timeout
import scala.concurrent.duration._


class Finder(mmu:ActorRef) extends Actor{
  val duration = Duration(5, SECONDS)
  implicit val timeout = Timeout(5 seconds)

  override def receive: Receive = {
    case Find(path: List[String], para: Seq[(String,String)]) =>{
      val respond = sender
      if(path.last.toString.matches("[0-9]*")){
        val nodeId=path.last
        val futureNode:Future[Node] = (mmu ? Get(nodeId)).mapTo[Node]
        futureNode.onSuccess {
          case node: Node => {
            val fields = node.getFields
            val edges = node.getEdges
            val resultList:mutable.MutableList[String]=mutable.MutableList(nodeId)
            if(para.isEmpty){
              fields.getAll.foreach(kv=>{
                resultList+=kv.getKey()
                resultList+=kv.getValue()
              })
            }else{
              para.foreach(p=>{
                val fieldsIndex = fields.find(p._1)
                val edgesIndex = edges.find(p._1)
                if (fieldsIndex != (-1)){
                  resultList+=p._1
                  resultList+=fields.get(fieldsIndex).getValue()
                }else if(edgesIndex != (-1)){
                  edges.getKeySet(edgesIndex).getValue().foreach(id=>{
                    resultList+=p._1
                    resultList+=id
                  })
                }else{
                  resultList+=p._1
                  resultList+="Not Found"
                }
              })
            }
            respond!resultList.toList
          }
        }
        futureNode.onFailure {
          case error=>respond!List(nodeId)
        }
        //              futureNode.onSuccess {
      }else{
        respond!List("Ilegel Path")
      }
    }
  }
}

//class Finder(mmu: ActorRef) extends Actor{
//  val duration = Duration(5, SECONDS)
//  implicit val timeout = Timeout(duration)
//  def receive = {
//    case Find(path, para, currentId, aes, iv) => {
//
//      println("In Find : " + currentId + " : "  + aes)
//
//      val respond = sender
//      //println("I need to find %s and %s".format(path.toString(),para.toString()))
//      var nodeId = ""
//      if(((!para.isEmpty)&&(!para(0)._2.equals(""))&&(para(0)._2.matches("[0-9]*")))||(path.last.matches("[0-9]*"))){
//        if((!para.isEmpty)&&(!para(0)._2.equals(""))&&(para(0)._2.matches("[0-9]*")))
//          nodeId = (para(0)._2)
//        else
//          nodeId = path.last
//
//        val permission= (mmu ? Permission(currentId, nodeId)).mapTo[Boolean]
//
//        permission.onSuccess{
//          case access: Boolean=>{
//            if(access){
//              //
//              val futureNode:Future[Node] = (mmu ? Get(nodeId)).mapTo[Node]
//              futureNode.onSuccess {
//                case node: Node => {
//                  val fields = node.getFields.decryption(aes, iv)
//                  val edges = node.getEdges
//                  if(para.isEmpty){
//                    respond ! new Object(fields.getAll().asInstanceOf[Array[Attribute]])
//                  }
//                  else {
//                    val Attributes: Array[KeyValue] = para.toArray.map(p => {
//                    val index = fields.find(p._1)
//                    if (index != (-1))
//                      fields.get(index)
//                    else
//                      new KeyValue(p._1, "404 Not Found")
//                    })
//                  //println("I GET RESULT")
//                  respond ! new Object(Attributes.asInstanceOf[Array[Attribute]])
//                  this.context.stop(self)
//                 }
//               }
//             }
//            futureNode.onFailure{
//              case exc => {
//                respond ! new Object(Array(new KeyValue(nodeId,"404 Not Found")))
//                this.context.stop(self)
//              }
//            }
//
//             //
//
//
//            }else{
//              respond ! new Object(Array(new KeyValue("id",nodeId)))
//              this.context.stop(self)
//            }
//          }
//        }
//
//        permission.onFailure{
//          case error=>{
//            respond ! new Object(Array(new KeyValue("id",nodeId)))
//            this.context.stop(self)
//          }
//        }
//
//
//
//
//        //println("to find %s".format(nodeId))
//
//      }
//      else{
//        var index = path.size-1
//        while(!path(index).matches("[0-9]*"))
//          index -= 1
//        nodeId = path(index)
//        val futureNode: Future[Node] = (mmu ? Get(nodeId)).mapTo[Node]
//        futureNode.onSuccess {
//          case node: Node => {
//            val fields = node.getFields
//            val edges = node.getEdges
//
//            val listOfFuture: List[Future[Object]] = edges.getKeySet(edges.find(path(index+1))).getValue().toList.map(id => {
//              var newPath = List(id)++path.drop(index+2)
//              val subActor = context.system.actorOf(Props(classOf[Finder],mmu))
//              (subActor ? Find(newPath, para, currentId, aes, iv)).mapTo[Object]
//            })
//            val futures: Future[List[Object]] = Future.sequence(listOfFuture).mapTo[List[Object]]
//            futures.onSuccess{
//              case result: List[Object] => respond! new Object(Array(new ArrayObject(path(index+1), result.toArray).asInstanceOf[Attribute]))
//            }
//            futures.onFailure{
//              case exc => respond ! new Object(Array(new KeyValue("Failure","404").asInstanceOf[Attribute]))
//            }
//
//          }
//        }
//        futureNode.onFailure{
//          case exc => {sender ! new Object(Array(new KeyValue("id", "404 Not Found")))}
//        }
//      }
//    }
//  }
//}

