package facebook

import akka.actor.{Props, Actor}
import org.json4s.DefaultFormats
import akka.actor.{ Actor, ActorRef, Props, ActorSystem}
import spray.routing.{Route, HttpService, RequestContext}
import akka.dispatch._
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern.ask
import spray.http.MediaTypes._
import spray.http._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import java.security.MessageDigest
import rsa._

class Operator extends Actor{
  implicit val timeout = Timeout(5 seconds)

  val json4sFormats = DefaultFormats
  
  //SHA256 hash function
  def sha256(prefix: String, random: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val plaintext = prefix + ";" + random
    md.update(plaintext.getBytes("UTF-8")); // Change this to "UTF-16" if needed
    val digest = md.digest()
    val hexString = new StringBuffer();
    for ( j <- 0 to digest.length-1) {
      val hex = Integer.toHexString(0xff & digest(j))
      if(hex.length() == 1)  hexString.append('0')  //hex number must be
      hexString.append(hex);
    }
    hexString.toString()
  } 
  
  def receive = {
//    case GetRequest(path, para, ctx, mmu, cache) =>{
//      println("Enter NormalGet == Do Something Later...")
//      println("Path : " + path)
//      for (i<- 0 to para.size-1)
//        println("(" + para(i)._1 + " : " + para(i)._2 +")")
//      // like ac=adhfjkahfjdfdab
//      val access = para(para.size-1)._2
//      val future1 = cache ? getAll(access)
//      val result = Await.result(future1, timeout.duration).asInstanceOf[(String, String, String)]
//
//      val currentId: String = result._1.toString()
//      val AESkey = result._2.toString()
//      val iv = result._3.toString()
//
//
//      println("Current Id == " + currentId)
//      println("Current AES Key == " + AESkey)
//
//      val finder: ActorRef = context.actorOf(Props(classOf[Finder], mmu))
//      // add input of AES key
//      val future = (finder ? Find(path, para.dropRight(1), currentId, AESkey, iv)).mapTo[Object]
//      future.onSuccess{
//        case result => {
//          ctx.complete(result.toString()+"\n")
//          this.context.stop(self)
//        }
//      }
//      future.onFailure{
//        case error => {
//          ctx.complete("Not Found\n")
//          this.context.stop(self)
//        }
//      }
//    }

    case GetRequest(path, para, ctx, mmu, cache, rsa) =>{
//      //println("Enter Normal Get ")
//      //println("Path : " + path)
//      for (i<- 0 to para.size-1)
//        println("(" + para(i)._1 + " : " + para(i)._2 +")")
//      //decrypt access token
      val access = rsa.decrypt(BigInt(para(para.size-1)._2), rsa.privateKey, rsa.modulus)


      val futureCurrentInfo = (cache ? getAll(access)).mapTo[(String,BigInt,BigInt)]
      futureCurrentInfo.onSuccess{
        case (currentId:String,key:BigInt,module:BigInt)=>{
          //println("GET has get the current user's information : currentId %s".format(currentId))
          if(path.last.toString.matches("[0-9]*")){
            val nodeId = path.last.toString
            //println("GET has get the current user's information : currentId s%".format(currentId))
            val futurePermission=(mmu?Permission(currentId, nodeId)).mapTo[Boolean]
            futurePermission.onSuccess{
              case haveRight: Boolean => {

                if(haveRight){
                 // println("GET has give the current user permission ")
                  val finder: ActorRef = context.actorOf(Props(classOf[Finder], mmu))
                  val futureInfo = (finder ? Find(path, para.dropRight(1))).mapTo[List[String]]
                  futureInfo.onSuccess{
                    //println("GET has give the current user permission ")
                    case info: List[String] => {
                      //println("GET get the infomation ")
                      val s = new StringBuilder
                      info.foreach(item => {
                        //RSA Encryption
                        s ++= rsa.encrypt(item,key,module).toString
                        s += ','
                      })

                      val result=s.dropRight(1).toString
                      //println("Operator Return Info of Get : " + result)
                      val entity = HttpEntity(result)
                      val res = HttpResponse(201, entity = entity)
                      ctx.complete(res)
                      this.context.stop(self)
                    }
                  }
                  futureInfo.onFailure{
                    case error=>{
                      //println("GET didn't get the infomation ")
                      val entity = HttpEntity(rsa.encrypt(nodeId,key,module).toString)
                      val res = HttpResponse(400, entity = entity)
                      ctx.complete(res)
                      this.context.stop(self)
                    }
                  }
                }
                else {
                  //println("GET deny the current user permission ")//no access right
                  val entity = HttpEntity(rsa.encrypt(nodeId,key,module).toString)
                  val res = HttpResponse(400, entity = entity)
                  ctx.complete(res)
                  this.context.stop(self)
                }
              }
            }

            futurePermission.onFailure{

              case error => {
                //println("GET deny the current user permission failure")
                val entity = HttpEntity(rsa.encrypt(nodeId,key,module).toString)
                val res = HttpResponse(400, entity = entity)
                ctx.complete(res)
                this.context.stop(self)
              }
            }

          }
          else{ //invalid path
            ctx.complete("Invalid Path\n")
            this.context.stop(self)
          }
        }
      }
      futureCurrentInfo.onFailure{
        case error => {
          ctx.complete("Invalid Access Token\n")
          this.context.stop(self)
        }
      }
    }
    
    case PostRequest(path, para, ctx, mmu, cache, rsa) =>{
      //println("Post Request")
      //var index = 0
      val p = path.reverse
      //val access = para(para.size-1)._2

      val access = rsa.decrypt(BigInt(para(para.size-1)._2), rsa.privateKey, rsa.modulus)


      val future1 = cache ? getId(access)
      val result = Await.result(future1, timeout.duration).asInstanceOf[String]
      val currentId: String = result.toString()
      val future2 = mmu ? Permission(currentId, path(0))
      val flag = Await.result(future2,  timeout.duration).asInstanceOf[Boolean]

      if (flag == true){
//         if (p(0).matches("[0-9]*")){  //update exist node
//            println("Enter IF")
//            mmu ! Update(p(0), para)
//            val res = HttpResponse(200)
//            ctx.complete(res)
//            this.context.stop(self)
//          }
//          else{                         //create new node
        var edge: Edges = null
        var t = ""
        if (p(0).equals("posts")){
          edge = new Edges(Array())
          t = "post"
        }
        else if (p(0).equals("albums")){
          edge = new Edges(Array("photos"))
          t = "album"
        }
        else if (p(0).equals("photos")){
          edge = new Edges(Array())
          t = "photo"
        }
        val array = ListBuffer[KeyValue]()
        array += (new KeyValue("id", ""))
        array += (new KeyValue("ownerId",currentId))
        array += (new KeyValue("type", t))
        for (i<- 0 to para.size-2){
          //println("Before dec -- (" + para(i)._1 + " : " + para(i)._2 + ")")
          //p2 RSA decrypt
          val e = rsa.decrypt(BigInt(para(i)._2), rsa.privateKey, rsa.modulus)
          //println("After dec -- (" + para(i)._1 + " : " + e + ")")
          array += (new KeyValue(para(i)._1, e))
        }
        val fields: Fields = new Fields(array.toArray)
        val future = mmu ? AddNode(fields, edge)
        val result = Await.result(future,  timeout.duration).asInstanceOf[(String, String)]
        mmu ! AddEdge(p(1), p(0), result._1.toString())


        ///
        val entity = HttpEntity("Created " + result._1 + "," + result._2)
        val res = HttpResponse(201, entity = entity)
        ctx.complete(res)
        this.context.stop(self)
          //}
      }
      else{
       // not be authorized
      }
    }
    
    case CreateUser(path, para, ctx, mmu, cache, rsa) =>{
      //update user node
      //println("~~~~Create User~~~~~~~~")
      //decrypt access token using Server Private key, BigInt from String may have exception
      val access = rsa.decrypt(BigInt(para(para.size-1)._2), rsa.privateKey, rsa.modulus)

      val future1 = cache ? getId(access)
      val result = Await.result(future1,  timeout.duration).asInstanceOf[String]
      val currentId: String = result.toString()
      val future2 = mmu ? Permission(currentId, path.reverse(0))
      val flag = Await.result(future2,  timeout.duration).asInstanceOf[Boolean]

      if (flag == true){
        val dePara:Seq[(String, String)]= para.map(p=>(p._1,rsa.decrypt(BigInt(p._2),rsa.privateKey,rsa.modulus)))
        val future = mmu ? Update(currentId, dePara.dropRight(1))
        val res = HttpResponse(201)

        //println("^^^^^^^^^^^ finish update ^^^^^^^^^")
        ctx.complete(res)
        this.context.stop(self)
      }
      else{
        // not be authorized
      }
    }
    
    case CreateEmptyUser(ctx, mmu, cache, para, rsa) =>{
      //println("In OP CreateUser mo : " + rsa.modulus.toString())
      val array = ListBuffer[KeyValue]()
      array += (new KeyValue("id", ""))
      array += (new KeyValue("type", "user"))
      array += (new KeyValue("name", ""))
      array += (new KeyValue("bod", ""))
      val fields: Fields = new Fields(array.toArray)
      val edge: Edges = new Edges(Array("friendlist", "posts", "albums"))


      val future = mmu ? AddNode(fields, edge)
      val result = Await.result(future,  timeout.duration).asInstanceOf[(String, String)]
      //generate access token
      val random = scala.util.Random.alphanumeric.take(8).mkString
      val access = sha256(result._1, random)


      val ClientPublic = BigInt.apply(para(0)._2)
      val modulus = BigInt.apply(para(1)._2)

      //println("@@In Create Empty User : " + "id - " + result + " ac - " + access + " pk - " + privatekey + " PC - " + ClientPublic + " modu - " + modulus)

      //val rsa = new RSA(1024)
      //RSA encrypt
      val r = rsa.encrypt(result._1.toString(), ClientPublic, modulus)
      val a = rsa.encrypt(access, ClientPublic, modulus)
      val pubServer = rsa.encrypt(rsa.publicKey.toString(), ClientPublic, modulus)

      //val pubMo = rsa.encrypt(rsa.modulus.toString(), ClientPublic, modulus)
      val pubMo = rsa.modulus

      //println("PKS : " + rsa.publicKey.toString())
      //println("P MO : " + rsa.modulus.toString())
      //println("E r : " + r + " E a : " + a + " E pk : " + pubServer + " E MO : "  + pubMo)

      //Send id, Accesstoken, publicServerKey, publicModulus
      val entity = HttpEntity("Created " + r + "," + a + "," + pubServer + "," + pubMo)

      //store access pair (id, accsess, pubC, modulus)
      cache ! Store(result._1.toString(), access, ClientPublic, modulus)

      val res = HttpResponse(201, entity = entity)

      ctx.complete(res)
      this.context.stop(self)

    }
    
    case RegisterDone(mmu, ctx) =>{
      //println("************Register Done***************")
      val future = mmu ? CreateFriendlist
      val result = Await.result(future,  timeout.duration).asInstanceOf[String]
      ctx.complete("FriendList Done")
    }
  }
}


