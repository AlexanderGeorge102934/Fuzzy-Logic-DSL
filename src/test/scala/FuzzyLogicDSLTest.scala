import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicDSL._

class FuzzyLogicDSLTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  // Clear the environment and global variables before each test
  override def beforeEach(): Unit = {
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()
  }

  behavior of "Fuzzy logic expressions and scoping"

  // Test addition of fuzzy variables with a cap of 1.0
  it should "correctly add fuzzy variables (capped at 1.0)" in {
    Scope(Gate("addGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.6))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
      Assign(Gate("addGate"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("addGate"))
    }

    EvaluateGateExpression("addGate") shouldBe 1.0
  }

  // Test addition of fuzzy variables
  it should "correctly add fuzzy variables" in {
    Scope(Gate("addGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.4))
      Assign(FuzzyVariable("B"), FuzzyValue(0.5))
      Assign(Gate("addGate"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("addGate"))
    }

    EvaluateGateExpression("addGate") shouldBe 0.9
  }

  // Test gate to correctly evaluate A in scope of a gate
  it should "correctly evaluate A in scope of addGate" in {
    Scope(Gate("addGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.6))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
      Assign(Gate("addGate"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("addGate"))
    }

    TestGate("addGate", "A") shouldBe 0.6
  }

  // Test gate to correctly evaluate A in scope of a gate
  it should "correctly evaluate B in scope of addGate" in {
    Scope(Gate("addGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.6))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
      Assign(Gate("addGate"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("addGate"))
    }

    TestGate("addGate", "B") shouldBe 0.7
  }

  // Test multiplication of fuzzy variables
  it should "correctly multiply fuzzy variables" in {
    Scope(Gate("multGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.5))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
      Assign(Gate("multGate"), MULT(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("multGate"))
    }

    EvaluateGateExpression("multGate") shouldBe 0.35
  }

  // Test complement operation on a fuzzy variable
  it should "correctly apply complement operation (1 - A)" in {
    Scope(Gate("complementGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.3))
      Assign(Gate("complementGate"), COMPLEMENT(FuzzyVariable("A")))(using Gate("complementGate"))
    }

    EvaluateGateExpression("complementGate") shouldBe 0.7
  }

  // Test AND operation (min(A, B))
  it should "correctly apply AND (min(A, B))" in {
    Scope(Gate("andGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.1))
      Assign(FuzzyVariable("B"), FuzzyValue(0.8))
      Assign(Gate("andGate"), AND(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("andGate"))
    }

    EvaluateGateExpression("andGate") shouldBe 0.1
  }

  // Test OR operation (max(A, B))
  it should "correctly apply OR (max(A, B))" in {
    Scope(Gate("orGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.5))
      Assign(FuzzyVariable("B"), FuzzyValue(0.2))
      Assign(Gate("orGate"), OR(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("orGate"))
    }

    EvaluateGateExpression("orGate") shouldBe 0.5
  }

  // Test XOR operation (|A - B|)
  it should "correctly apply XOR (|A - B|)" in {
    Scope(Gate("xorGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.4))
      Assign(FuzzyVariable("B"), FuzzyValue(0.6))
      Assign(Gate("xorGate"), XOR(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("xorGate"))
    }

    EvaluateGateExpression("xorGate") shouldBe (0.2 +- 1e-10)
  }

  // Test alpha cut operation on a fuzzy variable
  it should "correctly apply alpha cut (A >= 0.6)" in {
    Scope(Gate("alphaCutGate")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.5))
      Assign(Gate("alphaCutGate"), ALPHA_CUT(FuzzyVariable("A"), 0.6))(using Gate("alphaCutGate"))
    }

    EvaluateGateExpression("alphaCutGate") shouldBe 0.0
  }

  // Test global variable assignment and retrieval
  it should "assign global variables and retrieve them correctly" in {
    Assign(FuzzyVariable("X"), FuzzyValue(0.4))
    TestGate("global", "X") shouldBe 0.4
  }

  // Test scoping of variables within gates and access to global variables in gates
  it should "scope variables within gates and ensure global variables are accessible in gates" in {
    Assign(FuzzyVariable("X"), FuzzyValue(0.4))

    Scope(Gate("logicGate1")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.3))
      Assign(FuzzyVariable("B"), FuzzyValue(0.6))
      Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))
    }

    TestGate("logicGate1", "X") shouldBe 0.4
  }

  // Test restoration of environment after exiting an anonymous scope
  it should "restore original environment after exiting anonymous scope" in {
    Assign(FuzzyVariable("Y"), FuzzyValue(0.5))

    AnonymousScope {
      Assign(FuzzyVariable("Y"), FuzzyValue(0.9))
      TestGate("global", "Y") shouldBe 0.9
    }

    TestGate("global", "Y") shouldBe 0.5
  }

  // Test evaluation of expressions within anonymous scopes and check for out-of-scope variable access
  it should "evaluate expressions within anonymous scopes correctly and throw an exception for out-of-scope variables" in {
    AnonymousScope {
      Scope(Gate("anonGate")) {
        Assign(FuzzyVariable("A"), FuzzyValue(0.7))
        Assign(FuzzyVariable("B"), FuzzyValue(0.2))
        Assign(Gate("anonGate"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("anonGate"))
      }

      EvaluateGateExpression("anonGate") shouldBe (0.9 +- 1e-10)
    }

    // After the anonymous scope, accessing A should throw an exception
    a[Exception] should be thrownBy TestGate("anonGate", "A")
  }

  // Test reverting variables back to their original state after exiting anonymous scope
  it should "revert variables back to their original state after anonymous scope ends" in {
    Assign(FuzzyVariable("Z"), FuzzyValue(0.4))

    AnonymousScope {
      Assign(FuzzyVariable("Z"), FuzzyValue(0.8))
      TestGate("global", "Z") shouldBe 0.8
    }

    TestGate("global", "Z") shouldBe 0.4
  }

}
