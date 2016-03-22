package rsa

import scala.util.Random
//import math._
//
//class RSA() {
//
//  def main(args: Array[String]){
//    
//  }
//  
//  val keySize = 17
//  if(keySize <= 0 ) {
//    throw new NumberFormatException("Invalid number!")
//  }
//  val bitLength = keySize/8
//  val p = generatePrime(keySize/2)
//  val q = generatePrime(keySize/2)
//  val phi = (p.-(BigInt(1))).*(q.-(BigInt(1)))
//  val n = p.*(q)
//
//  var publicKey = BigInt(7)
//  publicKey = generatePublicKey()
//  val privateKey = publicKey.modInverse(phi)
//
//  def generatePublicKey(): BigInt = {
//    while (phi.gcd(publicKey).intValue() > 1) {
//      publicKey = publicKey.+(BigInt(2))
//    }
//    publicKey
//  }
//
//  def getPublicKey(): BigInt = {
//    publicKey
//  }
//  
//  def getPrivateKey(): BigInt = {
//    privateKey
//  }
//  
//  def generatePrime(bitLength: Int): BigInt = {
//    BigInt.probablePrime(bitLength, Random)
//  }
//
//  def encrypt(publicKey: BigInt, message: String): BigInt = {
//   BigInt.apply(message.getBytes).modPow(publicKey, n)
//  }
//  
//  def decrypt(privateKey: BigInt, cipher: BigInt): String = {
//    new String(cipher.modPow(privateKey, n).toByteArray)
//  }
//
// 
//  override def toString() : String = {
//    println("p: " + p)
//    println("q: " + q)
//    println("phi:" + phi)
//    println("n: " + n)
//    println("public: " + publicKey)
//    println("private: " + privateKey)
//    "RSA Algorithm in " + keySize + " bits."
//  }
//  
//  
//}




class RSA(val n: Int) {
  var random = new Random
  var modulus: BigInt = _
  var publicKey: BigInt = _
  var privateKey: BigInt = _

  private var p = BigInt.probablePrime(n / 2, random) //Generate primes
  private var q = BigInt.probablePrime(n / 2, random)
  private var phi = (p - 1) * (q - 1) //Create phi

  modulus = p * q
  publicKey = 0x10001 //Classic 2^16 + 1
  privateKey = publicKey.modInverse(phi)
  
  def encrypt(message: String, publicKey: BigInt, modulus: BigInt) = BigInt.apply(message.getBytes).modPow(publicKey, modulus) //Functions to handle encryption and decryption

  def decrypt(message: BigInt, privateKey: BigInt, modulus: BigInt) = new String(message.modPow(privateKey, modulus).toByteArray) //In order to use these with strings, you must cast into BigInt and back

  //override def toString = "Bitlength: " + n + "\npublic: 0x" + publicKey.toString(16) + "\nprivate: 0x" + privateKey.toString(16) + "\nmodulus: 0x" + modulus.toString(16)
  
}
