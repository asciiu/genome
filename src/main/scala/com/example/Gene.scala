package com.example

case class Operator()
/**
  * Created by bishop on 9/14/16.
  */
class Gene(
            val id: String,
            target: Double,
            op: Operator)(implicit discr: Discretization) {

//  val bits: Bitset
//
//  def +- (that: Gene, idx: GeneticIndices): Gene = {
//    val clonedBits = cloneBits(bits)
//
//    Range(gIdx.geneOpIdx, bits.size).foreach(n =>
//      if( that.bits.get(n) ) clonedBits.set(n)
//      else clonedBits.clear(n)
//    )
//    val valOp = decode(clonedBits)
//
//    Gene(id, valOp._1, valOp._2)
//  }
//
//  def ^ (gIdx: GeneticIndices): Gene = ^ (gIdx.geneOpIdx)
//  def ^ (idx: Int): Gene = {}
//  def decode(bits: BitSet): (Double, Operator)  { }
}
