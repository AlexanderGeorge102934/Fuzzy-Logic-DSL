import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class FuzzyLogicTest extends AnyFlatSpec with Matchers {
  "A fuzzy set" should "perform union correctly" in {
    val setA = FuzzySet(Map("x1" -> 0.2, "x2" -> 0.8))
    val setB = FuzzySet(Map("x1" -> 0.5, "x2" -> 0.6))

    val result = setA.union(setB)
    result.elements shouldEqual Map("x1" -> 0.5, "x2" -> 0.8)
  }

  "A fuzzy set" should "perform intersection correctly" in {
    val setA = FuzzySet(Map("x1" -> 0.2, "x2" -> 0.8))
    val setB = FuzzySet(Map("x1" -> 0.5, "x2" -> 0.6))

    val result = setA.intersection(setB)
    result.elements shouldEqual Map("x1" -> 0.2, "x2" -> 0.6)
  }
  
  



}
