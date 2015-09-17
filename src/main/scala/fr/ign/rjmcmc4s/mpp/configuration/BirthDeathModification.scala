package fr.ign.rjmcmc4s.mpp.configuration

import scala.collection.mutable.MutableList

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification

class BirthDeathModification[T](val birth: MutableList[T] = new MutableList[T], val death: MutableList[T] = new MutableList[T]) extends Modification {
  /**
   * Insert an object to the modification to be inserted into the configuration.
   * @param t
   *        a new object.
   */
  def insertBirth(t: T) = this.birth += t

  /**
   * Insert an object to the modification to be removed from the configuration.
   * @param t
   *        an object (it has to belong to the configuration)
   */
  def insertDeath(t: T) = this.death += t

  /**
   * @return the difference between the number of object to be inserted and the number to be removed
   *         from the configuration.
   */
  def deltaSize() = this.birth.length - this.death.length

  /**
   * Clear the modification.
   */
  def clear() = {
    this.birth.clear()
    this.death.clear()
  }

  def apply(conf: Configuration) = conf match {
    case seqConf: ListBufferConfiguration[T] => {
      this.death foreach (t => seqConf.remove(t))
      this.birth foreach (t => seqConf.insert(t))
    }
    case _ => {}
  }
//  def +(m: BirthDeathModification[T]) =
//    new BirthDeathModification(this.birth ++ m.birth, this.death ++ m.death)

//  def +(m: Modification) = m match {
//    case that: BirthDeathModification[T] => this + that
//    case _ => null
//  }
  override def toString = "birth = " + this.birth + " -- death = " + this.death
}