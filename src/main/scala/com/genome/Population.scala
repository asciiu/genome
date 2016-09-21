package com.genome

import scala.collection.mutable.{ListBuffer}
import scala.util.Random


class Population(size: Int) {
  val pool = ListBuffer.empty[Chromosome]

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
    val newPool = ListBuffer.empty[Chromosome]
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
          stdout(n1.toString, gen)
          solutionFound = true
        }

        if (total2.isDefined && total2.get == target) {
          stdout(n2.toString, gen)
          solutionFound = true
        }

        newPool.append(n1, n2)

      } while(!solutionFound && pool.length > 0)

      pool.appendAll(newPool)
      newPool.clear()
      gen += 1
    }

    gen
  }

  def stdout(solution: String, generations: Int) = {
    println(s"Found: $solution")
    println(s"gen: $generations \n")
  }

  /**
    * Select a member from this population. Uses
    * a predefined target number to assess the fitness score.
    *
    * @param target
    * @return
    */
  def selectMember(target: Double): Chromosome = {
    // this method uses a Roulette Wheel
    // explained here http://www.obitko.com/tutorials/genetic-algorithms/selection.php
    val sumFitness = pool.foldLeft(0.0)((a, c) => a + Fitness.score(c, target))

    val randomFitness = sumFitness * Random.nextDouble()

    val it = pool.iterator
    var found = false
    var selected = pool.head
    var sum = 0.0

    while (!found && it.hasNext) {
      val chromo = it.next

      sum += Fitness.score(chromo, target)

      if (sum >= randomFitness) {
        found = true
        selected = chromo
      }
    }

    pool -= selected

    selected
  }
}
