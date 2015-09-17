package fr.ign.rjmcmc4s.mpp.configuration

import scala.collection.mutable.ListBuffer

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification

class ListBufferConfiguration[T](val unaryEnergy: Any, val binaryEnergy: Any) extends Configuration with Iterable[T]{
//  type Modification = BirthDeathModification[T]
  var container: ListBuffer[T]= ListBuffer[T]()
  
  def deltaEnergy(modif: Modification): Double = 0
  def getEnergy(): Double = 0
  def insert(t: T) = {container += t}
  def remove(t: T) = {container -= t}
//  def size = container.size
  def apply(index: Int) = this.container.apply(index)
  def modification() = new BirthDeathModification[T]
  def iterator = container.iterator
  def clear = container.clear
  def copy = {
    var copy = new ListBufferConfiguration[T](unaryEnergy, binaryEnergy)
    copy.container = container.clone
    copy
  }
}