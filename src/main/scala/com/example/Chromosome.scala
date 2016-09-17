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
  var total = 0

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
    total = addUp
    if (total == target) score = 0
    else score = 1.0 / (target - total)
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
  def addUp: Int = {
    def charToDigit(char: Char): Int = {
      char-'0'.toInt
    }

    val decodedString = decodeChromo

    if (isValid) {
      // find the first number
      decodedString.find(c => c.isDigit) match {
        case Some(c) =>
          val index = decodedString.indexOf(c) + 1
          var total = c - '0'.toInt
          var nextNum = false
          var op = " "

          for (i <- index to decodedString.length - 1) {

            val ch = decodedString(i)

            // look for the next operator
            if (!nextNum && !ch.isDigit) {
              op = ch.toString
              nextNum = true
            } else if (nextNum && ch.isDigit) {

              val dig = charToDigit(ch)
              op match {
                case "+" => total += dig
                case "-" => total -= dig
                case "*" => total *= dig
                case "/" if dig != 0 => (total / dig)
                case _ =>
              }
              nextNum = false
            }

          }
          total

        case None => 0
      }
    } else {
      0
    }
  }

  def crossOver(other: Chromosome): Unit = {
    if (Random.nextDouble() < crossOverRate) {
      val pos = Random.nextInt(chromo.length-1)
      val substr1 = chromo.substring(pos)
      val substr2 = other.chromo.substring(pos)
      chromo.replace(pos, chromo.length, substr2)
      other.chromo.replace(pos, chromo.length, substr1)
    }
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
    val decodeStr = decodeChromo

    var num = true
    var valid = true
    var i = 0

    do {
      val c = decodeStr(i)
      // must be num-op-num-op-num
      if (num == !c.isDigit) valid = false

      if (i > 0 && c == '0' && decodeStr(i-1) == '/') valid = false

      num = !num
      i += 1
    } while (valid && i < decodeStr.length)

    if (!decodeStr.last.isDigit) valid = false

    valid
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

