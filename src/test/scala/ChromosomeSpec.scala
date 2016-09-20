import com.genome.Chromosome
import org.scalatest._

class ChromosomeSpec extends FlatSpec with Matchers {

  implicit val code = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/')
  val chrosomeLength = 5

  "New Chromosome" must "convert to string properly" in {
    val chromo = new Chromosome("62+5-")
    assert(chromo.toString == "62+5-")
  }

  it must "use the proper encoding" in {
    val chromo = new Chromosome("62+5-")
    assert(chromo.genes.toString == "01100010101001011011")
  }

  "Random Chromsome" should "have appropriate number of genes" in {
    val chromo = new Chromosome(chrosomeLength)
    val expected = chrosomeLength*4
    assert(chromo.genes.length == expected)
  }

  "Chromosome crossover" should "switch the appropriate chars" in {
    val chromo1 = new Chromosome(chrosomeLength)
    val chromo2 = new Chromosome(chrosomeLength)

    // TODO
    assert(true)
  }
}
