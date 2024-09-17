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

  "A fuzzy set" should "perform complement correctly 1" in {
    val setA = FuzzySet(Map("x1" -> 0.2, "x2" -> 0.8))
    val complementResult = setA.complement

    // To avoid float comparison
    val tolerance = 1e-9

    complementResult.elements.foreach { case (key, value) =>
      val expected = Map("x1" -> 0.8, "x2" -> 0.2)(key)
      value shouldEqual expected +- tolerance
    }
  }

  "A fuzzy set" should "perform complement correctly 2" in {
    val setA = FuzzySet(Map("x1" -> 0.35, "x2" -> 0.81))
    val complementResult = setA.complement

    // To avoid float comparison
    val tolerance = 1e-9

    complementResult.elements.foreach { case (key, value) =>
      val expected = Map("x1" -> 0.65, "x2" -> 0.19)(key)
      value shouldEqual expected +- tolerance
    }
  }





}
