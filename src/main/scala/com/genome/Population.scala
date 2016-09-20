package com.genome

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class Population(size: Int) {
  val pool = ArrayBuffer.empty[Chromosome]

  // TODO this doesn't belong
  val crossOverRate = 0.7
  val mutationRate = 0.001
  val chromoLength = 5
  implicit val code = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/')
  for (x <- 0 until size) pool.append(new Chromosome(chromoLength))

  /**
    * Performs the simple selection algorithm on
    * the population.
    *
    * @param target
    * @return the number of generations
    */
  def evolve(target: Double): Int = {
    val newPool = ArrayBuffer.empty[Chromosome]
    var solutionFound = false
    var gen = 0

    while (!solutionFound) {
      do {
        val n1 = selectMember(target)
        var n2 = selectMember(target)

        // cross over and mutate
        n2 = n1.crossOver(n2, crossOverRate)
        n1.mutate(mutationRate)
        n2.mutate(mutationRate)

        val total1 = Fitness.computeValue(n1)
        val total2 = Fitness.computeValue(n2)

        if (total1.isDefined && total1.get == target) {
          println(s"generations: $gen solution: $n1 total:${total1}")
          solutionFound = true
        }

        if (total2.isDefined && total2.get == target) {
          println(s"generations: $gen solution: $n2 total:${total2}")
          solutionFound = true
        }

        newPool.append(n1, n2)

      } while(!solutionFound && newPool.length < pool.length)

      pool.clear()
      pool.appendAll(newPool)
      newPool.clear()
      gen += 1
    }

    gen
  }

  /**
    * Select a member from this population. Uses
    * a predefined target number to assess the fitness score.
    *
    * @param target
    * @return
    */
  def selectMember(target: Double): Chromosome = {
    val totalFitness = pool.foldLeft(0.0)((a, c) => Fitness.score(c, target))
    val slice = totalFitness * Random.nextDouble()

    var ttot = 0.0
    var node = pool.last

    var found = false
    var i = pool.length - 1

    while (!found && i > 0) {
      val chromo = pool(i)
      ttot += Fitness.score(chromo, target)

      if (ttot > slice) {
        pool.remove(i)
        node = chromo
        found = true
      }
      i -= 1
    }

    if (pool.length > 0 && !found) pool.remove(pool.length - 1)
    node
  }
}
