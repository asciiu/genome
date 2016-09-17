package com.example

/**
  * Created by bishop on 9/14/16.
  */
//type Pool[T <: Gene] = ArrayBuffer[Chromosome[T]]
//
//class Population[T <: Gene](
//                             limit: Int,
//                             val chromosomes: Pool[T]) {
//
//  // selection operator
//  def select(score: Chromosome[T] => Unit, cutOff: Double) = {
//    val cumul = chromosomes./:(0.0)(
//      (s,x) =>{ score(xy); s + xy. unfitness}
//    )
//
//    chromosomes foreach( _ /= cumul)
//    // orders the population by decreasing value
//    val newChromosomes = chromosomes.sortWith(_.unfitness < _.unfitness)
//    // applies a soft limit function on population growth, cutOff
//    val cutOffSize = (cutOff*newChromosomes.size).floor.toInt
//    // reduces the size of the population to the lowest of the two limits:
//    // the hard limit, limit, or the soft limit,cutOffSize
//    val newPopSize = if(limit<cutOffSize) limit else cutOffSize
//
//    chromosomes.clear
//    chromosomes ++= newChromosomes.take(newPopSize)
//  }
//
//  // cross-over operator
//  def +- (xOver: Double): Unit = {
//    if( size > 1) {
//      val mid = size>>1
//      val bottom = chromosomes.slice(mid, size)
//
//      val gIdx = geneticIndices(xOver)
//      val offSprings = chromosomes.take(mid)
//        .zip(bottom)
//        .map(p => p._1 +-(p._2, gIdx))
//        .unzip
//
//      chromosomes ++= offSprings._1 ++ offSprings._2
//    }
//  }
//
//  // mutation operator
//  def ^ (mu: Double)
//  // ...
//}
