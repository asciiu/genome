package com.example

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object GeneticAlgorithm {
  val poolSize = 40
  val chromoLen = 5

  def evolve(target: Int) = {
    val pool = ArrayBuffer.empty[Chromosome]
    val newPool = ArrayBuffer.empty[Chromosome]

    for (x <- 0 until poolSize) pool.append(new Chromosome(target, chromoLen))

    var solutionFound = false
    var gen = 0

    while (!solutionFound) {
      newPool.clear()
      gen += 1

      for (x <- pool.length-1 to 0 by -2) {
        val n1 = selectMember(pool)
        var n2 = selectMember(pool)

        // cross over and mutate
        n2 = n1.crossOver(n2)

        n1.mutate
        n2.mutate

        n1.scoreChromo
        n2.scoreChromo

        if (n1.total == target && n1.isValid) {
          println(s"generations: $gen solution: ${n1.decodeChromo} total:${n1.total}")
          solutionFound = true
        }

        if (n2.total == target && n2.isValid) {
          println(s"generations: $gen solution: ${n2.decodeChromo} total:${n2.total}")
          solutionFound = true
        }

        newPool.append(n1, n2)
      }
      pool.appendAll(newPool)
    }
  }

  def selectMember(pool: ArrayBuffer[Chromosome]): Chromosome = {
    val poolFitness = pool.foldLeft(0.0)( (a, c) => c.score)
    val slice = poolFitness * Random.nextDouble()

    var ttot = 0.0
    var node = pool.last

    var found = false
    var i = pool.length-1

    while (!found && i > 0) {
      val chromo = pool(i)
      ttot += chromo.score
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

    var input = 0
    do {
      print("Enter a target (Int): ")
      input = scala.io.StdIn.readInt()

      evolve(input)
    } while (input != 0)
    // valid expression is Num op Num
    // therefore the expression is valid in cases where there are at least 2 nums and
    // a single op
  }
}
