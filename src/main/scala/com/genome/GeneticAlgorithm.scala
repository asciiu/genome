package com.genome

// http://www.ai-junkie.com/ga/intro/gat2.html
object GeneticAlgorithm {

  def main(args: Array[String]): Unit = {
    val pop = new Population(40)

    var input = 0
    do {

      print("Enter a target integer to calculate, and I will find the formula (0 to exit): ")
      input = scala.io.StdIn.readInt()
      pop.evolve(input)

    } while (input != 0)
  }
}
