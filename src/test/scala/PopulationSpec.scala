import com.genome.Population

import org.scalatest._

class PopulationSpec extends FlatSpec with Matchers {

  val pop = new Population(40)

  "Population" must "select random chromosome" in {
    val chromosome = pop.selectMember(7)
    assert(true)
  }
}