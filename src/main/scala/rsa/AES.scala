package rsa
 
import java.security.MessageDigest
import java.util
import javax.crypto.Cipher
import scala.util.Random
import javax.crypto.spec.SecretKeySpec
import javax.crypto.spec.IvParameterSpec
import org.apache.commons.codec.binary.Base64

object Main {
  def main(args: Array[String]): Unit={
    var msg = "Santa             "
    var mm = new AES() 
    val key = "60cc46088cd38b4a9d7bfe53a16b521ea86a1e87a6020d9f18abe6172888c16b"
    val iv = mm.genrateIV()
    val IVspec = mm.toArrayByte(iv)
    var en = mm.encrypt(key, msg, IVspec)
    println("en = "+en)
    var de = mm.decrypt(key, en, IVspec)
    println("de = "+de)
  }
}



class AES{
  
    val base : Array[Byte] = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p','q','r',
                                   's','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9')
    var iv: Array[Byte] = new Array[Byte](16)
    var ivspec = new IvParameterSpec(iv)

 def genrateIV() = {
   iv = new Array[Byte](16)
   for (i<-0 to 15){
      var rand = Random
      iv(i) = base(rand.nextInt(base.length))
    }
   new String(iv)
 }
 
 def toArrayByte(ivS: String): IvParameterSpec = {
    //println("~~~~~~~~~~~~~~~~~~~~~~~~~IV length" + ivS.getBytes().length)
    ivspec = new IvParameterSpec(ivS.getBytes())
    ivspec
 }

 def encrypt(key: String, value: String, ivspec: IvParameterSpec): String = {
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key), ivspec)
    Base64.encodeBase64String(cipher.doFinal(value.getBytes("UTF-8")))
  }

  def decrypt(key: String, encryptedValue: String, ivspec: IvParameterSpec): String = {
   // println("In decrypt :: " + key + " Valuse"  + encryptedValue)
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key), ivspec)
    new String(cipher.doFinal(Base64.decodeBase64(encryptedValue)))
  }

  def keyToSpec(key: String): SecretKeySpec = {
    var keyBytes: Array[Byte] = (SALT + key).getBytes("UTF-8")
    val sha: MessageDigest = MessageDigest.getInstance("SHA-1")
    keyBytes = sha.digest(keyBytes)
    keyBytes = util.Arrays.copyOf(keyBytes, 16)
    new SecretKeySpec(keyBytes, "AES")
  }

  private val SALT: String =
    "jMhKlOuJnM34G6NHkqo9V010GhLAqOpF0BePojHgh1HgNg8^72k"
  
}   
 



