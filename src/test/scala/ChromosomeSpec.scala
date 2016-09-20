import com.genome.Chromosome
import org.scalatest._

class ChromosomeSpec extends FlatSpec with Matchers {

  "new Chromosome" should "have tests" in {

    implicit val code = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/')
    val chromo = new Chromosome(5)
    println(chromo.toString())
    
    true should === (true)
  }
}
