package com.genome

/**
  * Created by bishop on 9/20/16.
  */
object Fitness {

  def computeValue(chromo: Chromosome): Option[Double] = {
    if (isValid(chromo)) {

      val decodedString = chromo.toString()
      val ops = decodedString.filter( !_.isDigit )
      val nums = decodedString.filter( _.isDigit ).map( _.toDouble - 48.0 )

      val value = ops match {
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
      Some(value)
    } else {
      None
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
    computeValue(chromo) match {
      case Some(value) if value == target => 1.0
      case Some(value) => 1.0 / (target - value)
      case None =>  0.0
    }
    //if (isValid(chromo)) {
    //  computeValue(chromo)

    //  if (total == target) 0.0
    //  else 1.0 / (target - total)

    //} else {
    //  0.0
    //}
  }
}
