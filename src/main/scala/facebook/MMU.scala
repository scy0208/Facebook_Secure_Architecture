package facebook

import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet


class MMU extends Actor{
  println("Memory Initialize ...")
  var memory = new HashMap[String, Node]()
  println("HashMap : " + memory.isEmpty)
  
  var id  = -1
  
  
  def permission(currentId:String, nodeId:String):Boolean={
    if(currentId==nodeId) return true
    val future=memory.get(nodeId)
    future match {
      case Some(node) =>{
        if(node.getEdges.find("friendlist")!= (-1)){
          //return node.getEdges.findEdge("friendlist", currentId)
          return false
        }else{
           val ownerId=node.getFields.getOwner()
           return permission(currentId, ownerId)
        }
      }
      case _ =>return false
    }
  }
  
  
  def receive = {
    case AddEdge(nodeId: String, edgeName: String, id: String) => 
      val future = memory.get(nodeId)
      future match{
        case Some(node) =>node.getEdges.addEdge(edgeName, id)
        case _ =>
      }
      
    case Permission(currentId:String, nodeId:String)=>{
      sender ! permission(currentId, nodeId)
    }
      
    case Get(nodeId: String) => {
      val node = memory.get(nodeId)
      node match{
        case Some(s) => sender ! s
        case _ => 
      }
      
    } 
    case AddNode(fields: Fields, edges: Edges) =>
      id += 1
      fields.addID(id.toString())
      memory += (id.toString() -> new Node(fields, edges))
      memory.foreach({ case (s, n)=> println(s.toString() + " : " + n.toString())})
      
      sender ! (id.toString, fields.getType)
      
    case Update(nodeId: String, para: Seq[(String,String)]) => {
      val fnode = memory.get(nodeId)
      println("******************In update user data")
      fnode match {
        case Some(node)=>{
          para.foreach(p=>node.getFields.update(p._1, p._2))
        }
        
        case _ =>
      }
       memory.foreach({ case (s, n)=> println(s.toString() + " : " + n.toString())})
    }
    
    case CreateFriendlist => {
      val userAmount = id
      //println("ENter CreateFriends .. user Amoutn : " + userAmount )
      for (i <- 1 to id){
        for (j <- 1 to 10){
          val fid = Random.nextInt(userAmount)
          val node = memory.get(i.toString()).get
          node.getEdges.addEdge("friendlist", fid.toString())
          val friend = memory.get(fid.toString()).get
          friend.getEdges.addEdge("friendlist", i.toString())
        }
      }
      sender ! "Finish"
    }
      
  }
}