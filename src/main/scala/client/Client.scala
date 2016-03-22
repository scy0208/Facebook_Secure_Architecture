package client

import java.security.MessageDigest
import javax.crypto.spec.IvParameterSpec

import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import akka.util.Timeout
import spray.client.pipelining.sendReceive
import spray.httpx.RequestBuilding
import spray.http.HttpResponse
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}
import util.Random
import rsa._

class SingleClient(behavior: String) extends Actor with RequestBuilding {
  val duration = Duration(60, SECONDS)
  implicit val requestTimeout = Timeout(duration)
  implicit val executionContext = context.dispatcher
  
  val clientPipeline = sendReceive
  var id = -1
  var album = -1
  var post = -1
  var photo = -1
  var accesstoken = ""
  var AESkey = ""
  var ServerPublicKey: BigInt = -1
  var ServerModulus: BigInt = -1

  val rsa = new RSA(2048)
  val ClientPublicKey = rsa.publicKey
  val ClientPrivateKey = rsa.privateKey
  val modulus = rsa.modulus

  var Record = new mutable.HashMap[String, (String, IvParameterSpec, String)]()
  //store (id, (AES, iv, Type))
  
  
  def to18(s: String): String = {
    if (s.length() % 18 != 0){
      val times = 18 - s.length() % 18
      val sb = new StringBuffer(s)
      for (i <- 1 to times)
        sb.append(" ");
      sb.toString()
    }
    else
       s
  }


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
    case PreRegister(se) => {
      println("CP+ " + ClientPublicKey + "  CP- " + ClientPrivateKey)
      println(" ==== Enter PreRegister")
      
      val fResponse: Future[HttpResponse] = clientPipeline {
       Get("http://127.0.0.1:8080/preregister?pc=" + ClientPublicKey+"&m="+modulus)
      }
      fResponse onComplete {
        case Success(response) =>  
         println(response.status.value)
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")  
          
          println("IN PreRegister : " + entity.toList)
          
          //decrypt
          id = rsa.decrypt(BigInt.apply(entity(0)),ClientPrivateKey,modulus).toInt
          println("client decrypt id : "+id + "\n")
          accesstoken = rsa.decrypt(BigInt.apply(entity(1)),ClientPrivateKey,modulus)
          println("client decrypt ac : "+ accesstoken + "\n")
          ServerPublicKey = BigInt(rsa.decrypt(BigInt.apply(entity(2)),ClientPrivateKey,modulus))
          println("client decrypt pk : "+ ServerPublicKey+ "\n")
          //ServerModulus = BigInt(rsa.decrypt(BigInt.apply(entity(3)),ClientPrivateKey,modulus))
          ServerModulus = BigInt.apply(entity(3))

          println("@@@@@@@@@@user id : " + id + " access toke : " + accesstoken + " PublicServerKey : "  + ServerPublicKey)
          println("Finish Key Exchange\n")

          se ! InitializeDone
        case Failure(error) => 
          println(error.getMessage)
      }
    }
    
    
    case Register(name, bod, se) => {
      println("Enter Register =======")
      val mm = new AES()
      val random = scala.util.Random.alphanumeric.take(8).mkString
      AESkey = sha256("xyf", random)
      val iv = mm.genrateIV()
      val ivspec = mm.toArrayByte(iv)


      val e0 = mm.encrypt(AESkey, to18(name), ivspec)
      val e1 = mm.encrypt(AESkey, to18(bod), ivspec)

      println("IN Register e0 is : " + e0 + " e1 : "  + e1)

      val nm = rsa.encrypt(mm.encrypt(AESkey, to18(name), ivspec), ServerPublicKey, ServerModulus)
      val bd = rsa.encrypt(mm.encrypt(AESkey, to18(bod), ivspec), ServerPublicKey, ServerModulus)
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)  //server public key to encrypt
      val url = "http://127.0.0.1:8080/page/" + id + "?name=" + nm + "&bod="+ bd + "&ac=" + ac

      println("IN Register URL is : " + url)


      val fResponse: Future[HttpResponse] = clientPipeline {
        Post(url)
      }
      //in response package, it should encapsulate user id late(access token)
      fResponse onComplete {
        case Success(response) => {
          //get info from entity
          //decrypt publicServerKey --> privateCkey
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          //record id -> AES+type
          println("IN Register : " + entity.toList)
          val NodeID = id.toString
          val Type = "user"

          Record += (NodeID -> (AESkey,ivspec,Type))

          se ! RegisterDone

        }
        case Failure(error) => {
          println(error.getMessage)
          self ! Register(name, bod,se)
        }
      }
      Thread.sleep(500)
    }
    
    case FinishAll(s) => {
      println("Enter Finishi ALL")
      val fResponse: Future[HttpResponse] = clientPipeline {
        Get("http://127.0.0.1:8080/registerdone")
      }
      fResponse onComplete {
        case Success(response) =>  
          println(response.status.value)
          s ! Done
        case Failure(error) => 
          println(error.getMessage)
          s ! Error("Not Finish Register")
      }
    }
    
    case TestPost1 =>{
      val name = "My_Album"
      val intro = "This_Album_Is_Great "
      println("Start build a new a album, name : %s, intro : %s".format(name,intro))

      val mm = new AES()
      val iv = mm.genrateIV()
      val ivspec = mm.toArrayByte(iv)

      //post Album
      val nm = rsa.encrypt(mm.encrypt(AESkey, to18(name), ivspec), ServerPublicKey, ServerModulus)
      val intr = rsa.encrypt(mm.encrypt(AESkey, to18(intro), ivspec), ServerPublicKey, ServerModulus)
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)  //server public key to encrypt

      val url1 = "http://127.0.0.1:8080/" + id + "/albums?name="+ nm + "&intro="+ intr + "&ac=" + ac
      println("URL is " + url1)
      val fResponse1 = clientPipeline {
        Post(url1)
      }
       fResponse1 onComplete {
        case Success(response) => {
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          //record id -> AES+type
          println("POST URL : " + entity.toList)
          val NodeID = entity(0)
          val Type = entity(1)

          Record += (NodeID -> (AESkey,ivspec,Type))
          album = NodeID.toInt
          self!TestGet1
        }
        case Failure(error) => println(error.getMessage)
      }
      //Thread.sleep(1000)
     // sender ! FinishIniPost
    }

    case TestGet1=>{
      println("Now we will get an album")
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)
      val Id = 0
      val url1 = "http://127.0.0.1:8080/" + Id + "albums/"+album+"?name&intro&ac=" + ac
      println("GET URL : %s".format(url1))
      val fResponse1 = clientPipeline {
        Get(url1)
      }
      fResponse1 onComplete {
        case Success(response) =>
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          rsa.decrypt(BigInt.apply(entity(0)),ClientPrivateKey,modulus)
          var deEntity:Array[String] = entity.map(item => rsa.decrypt(BigInt.apply(item),ClientPrivateKey,modulus))
          //println("Post get test : " + deEntity.toList)
          val itemId=deEntity.head
          val encryptinfo = Record.get(itemId)
          encryptinfo match {
            case Some(info) => {
              var result=new mutable.MutableList[(String,String)]
              val aesKey = info._1
              val iv=info._2
              val aes=new AES
              // val iv=aes.toArrayByte(iv_s)
              (0 until deEntity.size).foreach(i=>{
                if(i!=0 && i%2==0) {
                  result++=List((deEntity(i-1),aes.decrypt(aesKey,deEntity(i),iv).trim))
                }
              })
              println("Post get test : "+result.toList)
              self!TestPost2
            }
            case _ => {
              println("")
            }
          }
        case Failure(error) =>
          println("")
        //s ! Error("Not Finish Register")
      }
    }








    case TestPost2=>{
      val title = "Hello"
      val content = "World"
      println("Now we will post a message with title : %s content : %s".format(title,content))
      val mm = new AES()
      val iv = mm.genrateIV()
      val ivspec = mm.toArrayByte(iv)

      val ti = rsa.encrypt(mm.encrypt(AESkey, to18(title), ivspec), ServerPublicKey, ServerModulus)
      val co = rsa.encrypt(mm.encrypt(AESkey, to18(content), ivspec), ServerPublicKey, ServerModulus)
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)  //server public key to encrypt

      val url2 = "http://127.0.0.1:8080/" + id + "/posts?title="+ ti + "&content="+ co + "&ac=" + ac
      println("POST URL : " + url2)
      val fResponse2 = clientPipeline {
        Post(url2)
      }
      fResponse2 onComplete {
        case Success(response) => {
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          //record id -> AES+type
          println("post : " + entity.toList)
          val NodeID = entity(0)
          val Type = entity(1)
          Record += (NodeID -> (AESkey,ivspec,Type))
          post=NodeID.toInt
          self!TestGet2

        }
        case Failure(error) => println(error.getMessage)
      }
      //Thread.sleep(200)
    }

    case TestGet2 =>{
      println("Now we will get the post")
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)
      //val Id = 0
      val url1 = "http://127.0.0.1:8080/" + id + "posts/"+post+"?title&content&ac=" + ac
      val fResponse1 = clientPipeline {
        Get(url1)
      }
      fResponse1 onComplete {
        case Success(response) =>
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          rsa.decrypt(BigInt.apply(entity(0)),ClientPrivateKey,modulus)
          var deEntity:Array[String] = entity.map(item => rsa.decrypt(BigInt.apply(item),ClientPrivateKey,modulus))
          println("Post get test : " + deEntity.toList)
          val itemId=deEntity.head
          val encryptinfo = Record.get(itemId)
          encryptinfo match {
            case Some(info) => {
              var result=new mutable.MutableList[(String,String)]
              val aesKey = info._1
              val iv=info._2
              val aes=new AES
             // val iv=aes.toArrayByte(iv_s)
              (0 until deEntity.size).foreach(i=>{
                if(i!=0 && i%2==0) {
                  result++=List((deEntity(i-1),aes.decrypt(aesKey,deEntity(i),iv).trim))
                }
              })
              println("Post get test : "+result.toList)
              self!TestPost3
            }
            case _ => {
              println("")
            }
          }
        case Failure(error) =>
          println("")
          //s ! Error("Not Finish Register")
      }
    }

    case TestPost3=>{
      val title = "My_Photo"
      val time = "2015-12-11"
      println("Now we will post a photo with title : %s time : %s".format(title,time))
      val mm = new AES()
      val iv = mm.genrateIV()
      val ivspec = mm.toArrayByte(iv)

      val ti = rsa.encrypt(mm.encrypt(AESkey, to18(title), ivspec), ServerPublicKey, ServerModulus)
      val co = rsa.encrypt(mm.encrypt(AESkey, to18(time), ivspec), ServerPublicKey, ServerModulus)
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)  //server public key to encrypt

      val url2 = "http://127.0.0.1:8080/" + album+ "/photos?title="+ ti + "&time="+ co + "&ac=" + ac
      println("POST URL : " + url2)
      val fResponse2 = clientPipeline {
        Post(url2)
      }
      fResponse2 onComplete {
        case Success(response) => {
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          //record id -> AES+type
          println("post : " + entity.toList)
          val NodeID = entity(0)
          val Type = entity(1)
          Record += (NodeID -> (AESkey,ivspec,Type))
          photo=NodeID.toInt
          self!TestGet3
        }
        case Failure(error) => println(error.getMessage)
      }
      //Thread.sleep(200)

    }

    case TestGet3=>{
      println("Now we will get the photo")
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)
      //val Id = 0
      val url1 = "http://127.0.0.1:8080/" + album + "photos/"+photo+"?title&time&ac=" + ac
      val fResponse1 = clientPipeline {
        Get(url1)
      }
      fResponse1 onComplete {
        case Success(response) =>
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          rsa.decrypt(BigInt.apply(entity(0)),ClientPrivateKey,modulus)
          var deEntity:Array[String] = entity.map(item => rsa.decrypt(BigInt.apply(item),ClientPrivateKey,modulus))
          println("Post get test : " + deEntity.toList)
          val itemId=deEntity.head
          val encryptinfo = Record.get(itemId)
          encryptinfo match {
            case Some(info) => {
              var result=new mutable.MutableList[(String,String)]
              val aesKey = info._1
              val iv=info._2
              val aes=new AES
              // val iv=aes.toArrayByte(iv_s)
              (0 until deEntity.size).foreach(i=>{
                if(i!=0 && i%2==0) {
                  result++=List((deEntity(i-1),aes.decrypt(aesKey,deEntity(i),iv).trim))
                }
              })
              println("Post get test : "+result.toList)
              self!TestGet4
            }
            case _ => {
              println("")
            }
          }
        case Failure(error) =>
          println("")
        //s ! Error("Not Finish Register")
      }
    }

    case TestGet4=>{
      println("Now we will get the friendlist")
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)
      //val Id = 0
      val url1 = "http://127.0.0.1:8080/" + id +"?friendlist&ac=" + ac
      val fResponse1 = clientPipeline {
        Get(url1)
      }
      fResponse1 onComplete {
        case Success(response) =>
          val s = response.entity.asString.split(" ")
          val entity = s(s.size-1).trim().split(",")
          rsa.decrypt(BigInt.apply(entity(0)),ClientPrivateKey,modulus)
          var deEntity:Array[String] = entity.map(item => rsa.decrypt(BigInt.apply(item),ClientPrivateKey,modulus))
          println("Post get test : " + deEntity.toList)
//          val itemId=deEntity.head
//          val encryptinfo = Record.get(itemId)
//          encryptinfo match {
//            case Some(info) => {
//              var result=new mutable.MutableList[(String,String)]
//              val aesKey = info._1
//              val iv=info._2
//              val aes=new AES
//              // val iv=aes.toArrayByte(iv_s)
//              (0 until deEntity.size).foreach(i=>{
//                if(i!=0 && i%2==0) {
//                  result++=List((deEntity(i-1),aes.decrypt(aesKey,deEntity(i),iv).trim))
//                }
//              })
//              println("Post get test : "+result.toList)
//            }
//            case _ => {
//              println("")
//            }
//          }
        case Failure(error) =>
          println("")
        //s ! Error("Not Finish Register")
      }
    }
    
    case Simu =>{ 
      var postInterval= Duration(10, SECONDS)
      var getInterval= Duration(10, SECONDS)
      if (behavior.equals("Normal")){
       // do nothing
      }
      else if (behavior.equals("Crazy_Poster")){
        postInterval= Duration(5, SECONDS)
      }
      else if (behavior.equals("Crazy_Getter")){
        getInterval = Duration(5, SECONDS)
      }
      else if (behavior.equals("Slacker")){
        postInterval= Duration(30, SECONDS)
        getInterval = Duration(30, SECONDS)
      }
    context.system.scheduler.schedule(Duration(0,SECONDS), postInterval, self, CusPost)
      context.system.scheduler.schedule(Duration(10,SECONDS), getInterval, self, CusGet)
    }
    
    
    case CusPost => {      
      val PostIntructionPool = Seq[String]("/posts?title=GO&content=Gator", "/albums?name=Gator&intro=UF", "/albums/"+album+"/photos?name=nice")
      val url="http://127.0.0.1:8080/" + id + PostIntructionPool(Random.nextInt(PostIntructionPool.length)) + "&ac=" + accesstoken
      val fResponse = clientPipeline {
        Post(url)
      }
       fResponse onComplete {
        case Success(response) => 
          println("URL is " + url)
          println(response.entity.asString)
        case Failure(error) => println(error.getMessage)
      }      
    }
    
    case CusGet => {
      //val GetIntructionPool = Seq[String]("", "?id&name&bod&type", "/posts", "/posts?id&title&content", "/albums", "/albums?id&name&intro&type", "/albums/photos", "/albums/photos?id&name&type", "/friendlist")
      val GetIntructionPool = Seq[String]("/albums?name&intro")

      //acc encrypt PublicServerKey
      val ac = rsa.encrypt(accesstoken, ServerPublicKey, ServerModulus)  //server public key to encrypt
      val url="http://127.0.0.1:8080/" + id + GetIntructionPool(Random.nextInt(GetIntructionPool.length)) + "&ac=" + ac

      val fResponse = clientPipeline {
        Get(url)
      }
       fResponse onComplete {
        case Success(response) => 
          println("URL is " + url)
          println(response.entity.asString)
        case Failure(error) => println(error.getMessage)
      }
    }
  }
}