package com.example

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object TheThing {

  def computeValue(chromo: Chromosome): Double = {
    if (isValid(chromo)) {

      val decodedString = chromo.toString()
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

  def isValid(chromo: Chromosome): Boolean = {
    val str = chromo.toString()

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

  def score(chromo: Chromosome, target: Double): Double = {
    if (isValid(chromo)) {
      val total = computeValue(chromo)

      if (total == target) 0.0
      else 1.0 / (target - total)

    } else {
      0.0
    }
  }
}
// http://www.ai-junkie.com/ga/intro/gat2.html
object GeneticAlgorithm {
  val poolSize = 40

  //class Population(poolSize: Int) {
  //  val pool = ArrayBuffer.empty[Chromosome]
  //  for (x <- 0 until poolSize) pool.append(new Chromosome(chromoLen))
  //}

  def evolve(target: Double) = {
    val pool = ArrayBuffer.empty[Chromosome]
    val newPool = ArrayBuffer.empty[Chromosome]
    val chromoLength = 5

    implicit val code = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/')
    for (x <- 0 until poolSize) pool.append(new Chromosome(chromoLength))

    var solutionFound = false
    var gen = 0

    while (!solutionFound) {
      newPool.clear()
      gen += 1

      for (x <- pool.length-1 to 0 by -2) {
        val n1 = selectMember(pool, target)
        var n2 = selectMember(pool, target)

        // cross over and mutate
        n2 = n1.crossOver(n2, 0.7)

        n1.mutate(0.001)
        n2.mutate(0.001)

        val score1 = TheThing.score(n1, target)
        val score2 = TheThing.score(n2, target)

        val total1 = TheThing.computeValue(n1)
        val total2 = TheThing.computeValue(n2)

        if (total1 == target) {
          println(s"generations: $gen solution: $n1 total:${total1}")
          solutionFound = true
        }

        if (total2 == target) {
          println(s"generations: $gen solution: $n2 total:${total2}")
          solutionFound = true
        }

        newPool.append(n1, n2)
      }
      pool.appendAll(newPool)
    }
  }

  def selectMember(pool: ArrayBuffer[Chromosome], target: Double): Chromosome = {
    val poolFitness = pool.foldLeft(0.0)( (a, c) => TheThing.score(c, target))
    val slice = poolFitness * Random.nextDouble()

    var ttot = 0.0
    var node = pool.last

    var found = false
    var i = pool.length-1

    while (!found && i > 0) {
      val chromo = pool(i)
      ttot += TheThing.score(chromo, target)

      if (ttot > slice) {
        pool.remove(i)
        node = chromo
        found = true
      }
      i -= 1
    }

    if (pool.length > 0 && !found) pool.remove(pool.length-1)
    node
  }

  def main(args: Array[String]): Unit = {

  //  var input = 0
  //  do {
  //    print("Enter a target (Int): ")
  //    input = scala.io.StdIn.readInt()

  //    evolve(input)
  //  } while (input != 0)
    // valid expression is Num op Num
    // therefore the expression is valid in cases where there are at least 2 nums and
    // a single op
  }
}
