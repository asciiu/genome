package com.genome

/**
  * Created by bishop on 9/14/16.
  */
class Discretization(
                      toInt: Double => Int,
                      toDouble: Int => Double) {

  def this(R: Int) = this(
    (x: Double) => (x*R).floor.toInt, (n: Int) => n/R
  )
}

//def decode(bits: BitSet): (Double, Operator) =
//  (discr.toDouble(convert(geneBits.rValue, bits)),
//  op(convert(geneBits.rOp, bits))
//)
