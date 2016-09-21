import com.genome.Chromosome
import com.genome.Fitness
import org.scalatest._

class FitnessSpec extends FlatSpec with Matchers {

  implicit val code = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/')
  val chrosomeLength = 5

  "Fitness" must "score invalid chromosomes" in {
    val chromo = new Chromosome("62+5-")
    val score = Fitness.score(chromo, 9)
    score should equal (0)
  }

  it must "score valid chromosomes " in {
    val chromo = new Chromosome("6+3/1")
    val score = Fitness.score(chromo, 9)
    score should equal (1.0)
  }

  it must "score inversely proportional to the target" in {
    val chromo1 = new Chromosome("6+3/3")
    val chromo2 = new Chromosome("0+1*0")
    val score1 = Fitness.score(chromo1, 9)
    val score2 = Fitness.score(chromo2, 9)

   score1 should be > score2
  }
}