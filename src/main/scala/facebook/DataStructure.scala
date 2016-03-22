package facebook

import scala.collection.mutable
import scala.collection.mutable.{HashMap,HashSet}
import rsa._


abstract class Attribute

// one pair of Key-Value
class KeyValue(key: String, value: String) extends Attribute{
  private var k = key
  private var v = value
  
  def getKey(): String = k
  def getValue(): String = v
  
  def decryption(aesKey:String, iv: String):KeyValue={
    
    if(key=="id"||key=="ownerId"||key=="type"||value=="")
      return new KeyValue(this.key,this.value)
    else{
      println("aes: %s key: %s value: %s".format(aesKey,this.key,this.value))
      val mm = new AES()
      val IVspec = mm.toArrayByte(iv)
      val decValue = (mm.decrypt(aesKey, this.value, IVspec)).trim() 
      return new KeyValue(this.key,decValue)
    }
  }
  
  def setValue(value: String) = 
    this.v = value
  override def toString: String = 
    k + " : " + v
}

// one pair of Key-ValueSet
class KeySet(key: String, value: mutable.HashSet[String]) extends Attribute{
  private var k = key
  private var v = value
  
  def getKey(): String = k
  def getValue(): mutable.HashSet[String]= v
  
  def add(id: String): Unit =
    value += id 
  def find(id: String): Boolean = 
    value.contains(id)
    
  override def toString:String={
    var result=key+" : ["
    value.foreach(attribute=>{result+=(" "+attribute.toString+"\n")})
    result+="]"
    return result
  }
}

// encapsulate all normal attributes of one node
class Fields(attributes: Array[KeyValue]){
  def find(k: String): Int = {
    (0 until attributes.size).foreach(i => {if(attributes(i).getKey().equals(k)) return i})
    return -1
  }
  def getAll(): Array[KeyValue] = attributes
  def get(i: Int): KeyValue = attributes(i)
  def update(key: String, value: String) = {
    for (i <- 0 to attributes.size-1){
      if (attributes(i).getKey().equals(key)){
        attributes(i).setValue(value)
      }
    }
  }
  def getType():String={
    (0 until attributes.size).foreach(i=> {if(attributes(i).getKey().equals("type")) return attributes(i).getValue()})
    return "user"
  }

  def addID(id: String) = {
    (0 until attributes.size).foreach(i=>{if(attributes(i).getKey().equals("id")) {attributes(i).setValue(id)} })
  }
  
  def getOwner():String={
    (0 until attributes.size).foreach(i=>if(attributes(i).getKey().equals("ownerId")) return attributes(i).getValue())
    return "NONE"
  }
  
  def decryption(aesKey:String, iv: String):Fields={
    return new Fields(attributes.map { kv => kv.decryption(aesKey, iv) })
  }
  
  override def toString(): String={
    var result: String = "Fields: \n"
    attributes.foreach(member=>result+=(" "+member.toString+"\n"))
    return result
  }
  
  
}

// encapsulate all edge attributes of one node
class Edges(edgeNames: Array[String]){
  var members = edgeNames.map(edgeName => new KeySet(edgeName,new HashSet[String]()))
  def find(k: String): Int = {
    (0 until members.size).foreach(i => {if(members(i).getKey().equals(k)) return i})
    return -1
  }
  def findEdge(k: String, id: String): Boolean = {
    val index = find(k)
    if(index != (-1)){
      return members(index).find(id)
    }
    return false
  }
  def getKeySet(i: Int): KeySet = {
    return members(i)
  }
  def addEdge(k: String, id: String): Unit ={
    (0 until members.size).foreach(i=> {
      if(members(i).getKey() == k){
        members(i).add(id)
      }
    })
  }
  def getMembers(): Array[KeySet]={
    return members
  }
  override def toString(): String={
    var result: String = "Edges: \n"
    members.foreach(member=>result+=(" "+member.toString+"\n"))
    return result
  }
  
}


//single node in facebook (page, album, photo ...)
class Node(fields:Fields, edges:Edges){
  def getFields: Fields =  fields
  def getEdges: Edges = edges
  override def toString() = this.getFields.toString() + "\n" + this.getEdges.toString()
}



// display
class Object(attributes: Array[Attribute]){
  override def toString:String={
      var result="{"
      attributes.foreach(member=>{result+=(" "+member.toString+"\n")})
      result+=" }"
      return result
  }
}

class ArrayObject(key: String, value: Array[Object]) extends Attribute{
  override def toString:String={
    var result=key+" : ["
    value.foreach(attribute=>{result+=(" "+attribute.toString+"\n")})
    result+="]"
    return result
  }
}
