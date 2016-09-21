package com.genome

import scala.util.Random

/**
  * Each chromosome must be declared with a length and an implicit array
  * that engeness the genes.
  */
class Chromosome(length: Int)(implicit code: Array[Char]) {
  private val genes = new StringBuffer(length*4)

  // Randomly generate genes for this chromosome
  0 until length foreach { _ =>
    val random = Random.nextInt(code.length)
    val binString = "%4s".format(Integer.toBinaryString(random)).replace(' ', '0')
    genes.append(binString)
  }

  def this(genes: String)(implicit code: Array[Char]) {
    this(genes.length)
    this.genes.delete(0, this.genes.length)

    for (gene <- genes) {
      val index = code.indexOf(gene)
      if (index > -1) {
        val binString = "%4s".format(Integer.toBinaryString(index)).replace(' ', '0')
        this.genes.append(binString)
      }
    }
  }

  override def toString(): String = {
    val buffer = new StringBuffer(length*4)
    for (x <- 0 to genes.length-4 by 4) {
      val gene = genes.substring(x, x+4)

      val idx = Integer.parseInt(gene, 2)
      if (idx < code.length) buffer.append(code(idx))
    }
    buffer.toString
  }

  def crossOver(other: Chromosome, rate: BigDecimal) = {
    if (Random.nextDouble() < rate) {
      val pos = Random.nextInt(genes.length-1)

      val substr1 = genes.substring(pos)
      val substr2 = other.genes.substring(pos)
      genes.replace(pos, genes.length, substr2)
      other.genes.replace(pos, genes.length, substr1)
    }
  }

  def mutate(rate: BigDecimal) = {
    for (x <- 0 to genes.length()-1) {
      if (Random.nextDouble() <= rate) {
        val c = if (genes.charAt(x) == '0') '1' else '0'
        genes.setCharAt(x, c)
      }
    }
  }
}

