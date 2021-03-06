package fr.ign.rjmcmc4s

import scala.xml.XML

class Parameters(fileName: String) {
  val xml = XML.loadFile(fileName)
  def get(name: String) = {
    ((xml \\ "param").find (n => (n \ "@key").text == name).get \ "@value").text
  }
  def getDouble(name: String) = java.lang.Double.parseDouble(get(name))
  def getInt(name: String) = java.lang.Integer.parseInt(get(name))
}