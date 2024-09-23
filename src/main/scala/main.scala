import scala.collection.mutable
// Has to support both a value or a set as input 

// Define an environment for variable storage
type Environment = mutable.Map[String, mutable.Map[String, Double]] // Map from gate name to variables

object FuzzyLogicDSL:

  // Implicit environment for scoping variables within gates
  given env: Environment = mutable.Map()

  // Base class for fuzzy expressions
  abstract class FuzzyExpr:
    def eval: Double

  // Concrete classes for different operations
  case class FuzzyValue(v: Double) extends FuzzyExpr:
    def eval: Double = v

  case class FuzzyVariable(name: String) extends FuzzyExpr:
    def eval: Double =
      // Fetch the value from the environment based on the current gate's scope
      summon[Environment].collectFirst {
        case (_, scope) if scope.contains(name) => scope(name)
      }.getOrElse(throw new Exception(s"Variable $name not found in any scope"))

  // Operations like addition, multiplication, etc.
  case class FuzzyAdd(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval: Double = math.min(1.0, e1.eval + e2.eval)

  case class FuzzyMult(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval: Double = e1.eval * e2.eval

  case class FuzzyComplement(e: FuzzyExpr) extends FuzzyExpr:
    def eval: Double = 1.0 - e.eval

  case class FuzzyAND(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval: Double = math.min(e1.eval, e2.eval) // AND is min

  case class FuzzyOR(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval: Double = math.max(e1.eval, e2.eval) // OR is max

  case class FuzzyXOR(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval: Double = math.max(e1.eval, e2.eval) - math.min(e1.eval, e2.eval) // XOR formula

  case class FuzzyAlphaCut(e: FuzzyExpr, alpha: Double) extends FuzzyExpr:
    def eval: Double = if e.eval >= alpha then e.eval else 0.0 // Alpha Cut

  // Class for holding gates
  case class Gate(name: String)

  // Gate system that stores gates
  case class GateSystem(gates: mutable.Map[String, FuzzyExpr] = mutable.Map())

  // Global mutable gate system
  val gateSystem = GateSystem()

  // Assign variables to values or gates to expressions
  // Assignment function for variables within a specific gate's scope
  def Assign(left: Any, right: FuzzyExpr)(using gate: Gate): Unit = left match
    // Case 1: Assign a value to a FuzzyVariable within the current gate's scope
    case FuzzyVariable(name) =>
      summon[Environment]
        .getOrElseUpdate(gate.name, mutable.Map[String, Double]())
        .update(name, right.eval)

    // Case 2: Assign a fuzzy expression to a Gate (store it in the gate system)
    case gate: Gate =>
      gateSystem.gates(gate.name) = right

    case _ =>
      throw new Exception("Invalid assignment")

  // Scope function that defines variable bindings within the context of a logic gate
  def Scope(gate: Gate)(block: Gate ?=> Unit): Unit =
    given Gate = gate
    block

  // Anonymous Scope Function: Executes block without assigning variables
  def AnonymousScope(block: => Unit): Unit =
    // Save the current environment state
    val currentEnv = summon[Environment].clone()

    // Execute the block within the anonymous scope
    try {
      block // Execute the anonymous block without assigning variables
    } finally {
      // Restore the environment after execution, discarding any changes
      summon[Environment].clear()
      summon[Environment].addAll(currentEnv)
    }


  // Test the logic gate result using the active bindings in the gate's context
  def TestGate(gateName: String, variableName: String): Double =
    // Fetch the logic gate expression from the gate system
    val gateExpr = gateSystem.gates.getOrElse(gateName, throw new Exception(s"Gate $gateName not found"))

    // Fetch the variable value from the specific gate's context
    val gateScope = summon[Environment].getOrElse(gateName, throw new Exception(s"No scope found for gate $gateName"))

    gateScope.getOrElse(variableName, throw new Exception(s"Variable $variableName not found in gate $gateName"))

  // Functions for fuzzy operations
  def ADD(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyAdd(e1, e2)
  def MULT(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyMult(e1, e2)
  def COMPLEMENT(e: FuzzyExpr): FuzzyExpr = FuzzyComplement(e)
  def AND(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyAND(e1, e2)
  def OR(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyOR(e1, e2)
  def XOR(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyXOR(e1, e2)
  def ALPHA_CUT(e: FuzzyExpr, alpha: Double): FuzzyExpr = FuzzyAlphaCut(e, alpha)


object Main:
  import FuzzyLogicDSL.*

  def main(args: Array[String]): Unit =
    println("Running Scope and Assignment Tests")

    // Normal Scope (variables will persist)
    Scope(Gate("logicGate1")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.5))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
    }

    Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"),FuzzyVariable("B")))(using Gate("logicGate1"))

    println(s"TestGate result for A in logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.5

    // Anonymous Scope (no variable assignments, only expression evaluations)
    AnonymousScope {
      // Assign variables in tempGate before testing it
      Scope(Gate("tempGate")) {
        Assign(FuzzyVariable("tempVar"), FuzzyValue(0.11)) // Assign a value to tempVar
      }

      // You can evaluate an expression within the scope
      Assign(Gate("tempGate"), ADD(MULT(FuzzyValue(0.9), FuzzyValue(0.2)), FuzzyValue(0.3)))(using Gate("tempGate"))

      // Testing the expression via a gate
      println(s"Anonymous Scope tempGate = ${TestGate("tempGate", "tempVar")}") // Should return the value for tempVar
    }


    // Outside the anonymous scope, the environment should be unchanged
    println(s"TestGate result for A in logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.5

    println("Running more complex gate evaluations")

    // Example of gate logic assignment with fuzzy expressions
    Scope(Gate("logicGate1")) {
      Assign(Gate("logicGate1"), ADD(MULT(FuzzyVariable("A"), FuzzyValue(0.2)), FuzzyValue(0.3)))(using Gate("logicGate1"))
    }

    println(s"TestGate result for logicGate1 = ${TestGate("logicGate1", "A")}")
//    
//    //Anonymous scoping shouldn't have specific names for the scope meaning anything within 
//
//
//
//    // 2. Assign a logic gate with a fuzzy expression that uses A and B
//    Assign(Gate("logicGate1"), ADD(MULT(FuzzyVariable("A"), FuzzyValue(0.2)), FuzzyValue(0.3)))(using Gate("logicGate1"))
//
//    // Test the logic gate result with the input A, should return 0.5
//    println(s"TestGate result for A = ${TestGate("logicGate1", "A")}") // Expected value is 0.5
//
//    // Test the logic gate result with the input B, should return 0.7
//    println(s"TestGate result for B = ${TestGate("logicGate1", "B")}") // Expected value is 0.7
//
//    // 3. Test composite gate
//    Scope(Gate("compositeGate")) {
//      Assign(FuzzyVariable("A"), FuzzyValue(0.9))
//      Assign(FuzzyVariable("B"), FuzzyValue(0.1))
//    }
//    Assign(Gate("compositeGate"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("compositeGate"))
//
//    // Test composite gate result
//    println(s"TestGate result for A in compositeGate = ${TestGate("compositeGate", "A")}") // Expected value is 0.9
//    println(s"TestGate result for B in logicGate1 = ${TestGate("logicGate1", "B")}") // Expected value is 0.7