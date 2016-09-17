package com.example

import scala.util.Random

object GeneEncoding {
  val ltable = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/')
}
/**
  * Created by bishop on 9/14/16.
  */
class Chromosome(target: Int, chromoLen: Int) {
  // TODO where should this go?
  val crossOverRate = 0.7
  val mutationRate = 0.001
  val chromo = new StringBuffer(chromoLen*4)
  //val decodeChromo = new StringBuffer(chromoLen*4)
  var score = 0.0
  var total = 0.0

  randomInit
  scoreChromo
  // example chromo generated from this method is
  // 00100100000000100001
  def randomInit = {
    // create the full buffer
    for (y <- 0 to chromoLen) {
      val pos = chromo.length
      val random = Random.nextInt(GeneEncoding.ltable.length)
      val binString = Integer.toBinaryString(random)
      val fillLen = 4 - binString.length

      for (x <- 0 to fillLen - 1) {
        chromo.append('0')
      }
      chromo.append(binString)
    }
  }

  def scoreChromo = {

    if (isValid) {
      total = addUp
      if (total == target) score = 0.0
      else score = 1.0 / (target - total)
    } else {
      total = 0
      score = 0.0
    }
  }

  def decodeChromo: String = {
    val buffer = new StringBuffer(chromoLen*4)
    for (x <- 0 to chromo.length-5 by 4) {
      val gene = chromo.substring(x, x+4)

      val idx = Integer.parseInt(gene, 2)
      if (idx < GeneEncoding.ltable.length) buffer.append(GeneEncoding.ltable(idx))
    }
    buffer.toString
  }

  // TODO this needs to be fixed to honor the order of operations
  def addUp: Double = {
    if (isValid) {

      val decodedString = decodeChromo
      val ops = decodedString.filter( !_.isDigit )
      val nums = decodedString.filter( _.isDigit ).map( _.toDouble - 48.0 )

      ops match {
        case "**" => nums(0) * nums(1) * nums(2)
        case "*/" => nums(0) * nums(1) / nums(2)
        case "*+" => nums(0) * nums(1) + nums(2)
        case "*-" => nums(0) * nums(1) - nums(2)
        case "*" => nums(0) * nums(1)

        case "//" => nums(0) / nums(1) / nums(2)
        case "/*" => nums(0) / nums(1) * nums(2)
        case "/+" => nums(0) / nums(1) + nums(2)
        case "/-" => nums(0) / nums(1) - nums(2)
        case "/" => nums(0) / nums(1)

        case "++" => nums(0) + nums(1) + nums(2)
        case "+*" => nums(0) + nums(1) * nums(2)
        case "+/" => nums(0) + nums(1) / nums(2)
        case "+-" => nums(0) + nums(1) - nums(2)
        case "+" => nums(0) + nums(1)

        case "--" => nums(0) - nums(1) - nums(2)
        case "-*" => nums(0) - nums(1) * nums(2)
        case "-/" => nums(0) - nums(1) / nums(2)
        case "-+" => nums(0) - nums(1) + nums(2)
        case "-" => nums(0) - nums(1)
        case _ if nums.length > 0 => nums(0)
      }
    } else {
      0
    }
  }

  def crossOver(other: Chromosome): Chromosome = {
    if (Random.nextDouble() < crossOverRate) {
      val pos = Random.nextInt(chromo.length-1)
      val substr1 = chromo.substring(pos)
      val substr2 = other.chromo.substring(pos)
      chromo.replace(pos, chromo.length, substr2)
      other.chromo.replace(pos, chromo.length, substr1)
    }

    other
  }

  def mutate = {
    for (x <- 0 to chromo.length()-1) {
      if (Random.nextDouble() <= mutationRate) {
        val c = if (chromo.charAt(x) == '0') '1' else '0'
        chromo.setCharAt(x, c)
      }
    }
  }

  def isValid: Boolean = {
    val str = decodeChromo
    if (str.length != 5) false
    else {

      var valid = true
      if (str(0).isDigit && !str(1).isDigit && str(2).isDigit && !str(3).isDigit && str(4).isDigit) {
        if (str(1) == '/') valid = str(2) != '0'
        if (str(3) == '/' && valid) valid = str(4) != '0'
      } else {
        valid = false
      }
      valid
    }
  }
}
//final class Chromosome[T <: Gene](val code: List[T]) {
//
//  var unfitness: Double = 1000*(1.0 + Random.nextDouble)
//  val chromosomeSize = code.length

//  def spliceGene(gIdx: GeneticIndices, thatCode: T): (T, T) = {
//    ((code(gIdx.chOpIdx) +- (thatCode, gIdx)),
//      (thatCode +- (code(gIdx.chOpIdx), gIdx)) )
//  }
//
//  // cross-over
//  def +-(
//          that: Chromosome[T],
//          gIdx: GeneticIndices
//        ): (Chromosome[T], Chromosome[T]) = {
//
//    val xoverIdx = gIdx.chOpIdx
//    val xGenes = spliceGene(gIdx, that.code(xoverIdx) )
//
//    val offSprng1 = code.slice(0, xoverIdx) ::: xGenes._1
//    :: that.code.drop(xoverIdx+1)
//
//    val offSprng2 = that.code.slice(0, xoverIdx) ::: xGenes._2
//    :: code.drop(xoverIdx+1)
//
//    (Chromosome[T](offSprng1), Chromosome[T](offSprng2)
//  }
//
//  // mutation
//  def ^ (gIdx: GeneticIndices): Chromosome[T] = {}
//
//  // normalization of fitness
//  def /= (normalizeFactor: Double): Unit = {}

//  def geneticIndices(prob: Double): GeneticIndices = {
//    var idx = (prob*chromosomeSize).floor.toInt
//
//    val chIdx =
//      if(idx == chromosomeSize)
//        chromosomeSize-1
//      else
//        idx
//
//    idx = (prob*geneSize).floor.toInt
//    val gIdx = if(idx == geneSize) geneSize-1 else idx
//
//    GeneticIndices(chIdx, gIdx)
//  }
//}

