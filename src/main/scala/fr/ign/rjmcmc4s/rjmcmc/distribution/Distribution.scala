package fr.ign.rjmcmc4s.rjmcmc.distribution

trait Distribution[T] {
  def pdf(n: T): Double
  def pdfRatio(n0: T, n1: T): Double
  def sample: T
}