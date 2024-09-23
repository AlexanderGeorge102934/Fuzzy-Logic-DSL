import scala.collection.mutable

// Environment for variable storage
type Environment = mutable.Map[String, mutable.Map[String, Double]] // Map from gate name to variables
type GlobalEnvironment = mutable.Map[String, Double] // Global variable environment

object FuzzyLogicDSL:

  // Implicit environment for scoping variables within gates
  given env: Environment = mutable.Map()
  val globalEnv: GlobalEnvironment = mutable.Map() // Global environment for unscoped variables

  // Base class for fuzzy expressions
  abstract class FuzzyExpr:
    def eval: Double

  // Concrete classes for different operations
  case class FuzzyValue(v: Double) extends FuzzyExpr:
    def eval: Double = v

  case class FuzzyVariable(name: String) extends FuzzyExpr:
    def eval: Double =
      // Try fetching the value from the environment based on the current gate's scope
      summon[Environment].collectFirst {
        case (_, scope) if scope.contains(name) => scope(name)
      }.getOrElse(
        // If not found in the scoped environment, fallback to the global environment
        globalEnv.getOrElse(name, throw new Exception(s"Variable $name not found in any scope or globally"))
      )

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

  // Assignment function for variables within a specific gate's scope or globally
  def Assign(left: Any, right: FuzzyExpr)(using gate: Gate = Gate("global")): Unit = left match
    // Case 1: Assign a value to a FuzzyVariable within the current gate's scope
    case FuzzyVariable(name) =>
      if gate.name == "global" then
        globalEnv.update(name, right.eval) // Global assignment
      else
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

  // Anonymous Scope Function: Executes block and restores specific gate environment and global environment after execution
  def AnonymousScope(block: => Unit): Unit =
    // Save the current environment state for specific gates by deep cloning the environment
    val originalEnv = summon[Environment].map { case (k, v) => k -> v.clone() }

    // Save the current state of the global environment
    val originalGlobalEnv = globalEnv.clone()

    // Execute the block within the anonymous scope
    try {
      block // Execute the anonymous block
    } finally {
      // Restore the specific environment of each gate after execution
      summon[Environment].clear()
      summon[Environment].addAll(originalEnv) // Reset to the original environment

      // Restore the global environment
      globalEnv.clear()
      globalEnv.addAll(originalGlobalEnv) // Reset to the original global environment
    }


  // Test the logic gate result using the active bindings in the gate's context or globally
  def TestGate(gateName: String, variableName: String): Double =
    // Check if a gate exists and the variable is scoped within that gate
    if summon[Environment].contains(gateName) then
      summon[Environment](gateName).get(variableName) match {
        case Some(value) => value
        case None =>
          // Fallback to global scope if the variable is not found in the gate
          globalEnv.getOrElse(variableName, throw new Exception(s"Variable $variableName not found globally or in gate $gateName"))
      }
    else
      // If the gate doesn't exist, fallback to the global scope directly
      globalEnv.getOrElse(variableName, throw new Exception(s"Variable $variableName not found globally or in gate $gateName"))

  // Evaluate the expression assigned to the gate, if any
  def EvaluateGateExpression(gateName: String): Double =
    gateSystem.gates.get(gateName) match {
      case Some(expr) => expr.eval
      case None => throw new Exception(s"No expression assigned to gate $gateName")
    }


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

    // Assigning variables without a scope to global values
    Assign(FuzzyVariable("X"), FuzzyValue(0.2)) // Global assignment
    Assign(FuzzyVariable("Y"), FuzzyValue(0.4)) // Global assignment

    println(s"Global X = ${TestGate("global", "X")}") // Should return 0.2
    println(s"Global Y = ${TestGate("global", "Y")}") // Should return 0.4



    // Normal Scope (variables will persist)
    Scope(Gate("logicGate1")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.5))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
    }


    Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))

    println(s"TestGate result for A in logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.5
    println(s"TestGate result for Global X in logicGate1 = ${TestGate("logicGate1", "X")}") // Should return 0.2
    println(s"Expression result for logicGate1 A + B = ${EvaluateGateExpression("logicGate1")}") // Should evaluate A + B = 0.5 + 0.7

    // Anonymous Scope (no variable assignments, only expression evaluations)
    AnonymousScope {
      Assign(FuzzyVariable("Y"), FuzzyValue(0.90)) // Global assignment
      println(s"Global Y in Anon Scope = ${TestGate("global", "Y")}") // Should return 0.9

      // Assign variables in tempGate before testing it
      Scope(Gate("tempGate")) {
        Assign(FuzzyVariable("tempVar"), FuzzyValue(0.11)) // Assign a value to tempVar
      }

      // Inside anonymous scope, modifying A for logicGate1 (this should not persist after scope ends)
      Scope(Gate("logicGate1")) {
        Assign(FuzzyVariable("A"), FuzzyValue(0.29))
      }

      println(s"Expression result for logicGate1 A + B (A is now 0.29, Anon Scope) = ${EvaluateGateExpression("logicGate1")}") // Should evaluate A + B = 0.29 + 0.7

      // You can evaluate an expression within the scope
      Assign(Gate("tempGate"), ADD(MULT(FuzzyValue(0.9), FuzzyValue(0.2)), FuzzyValue(0.3)))(using Gate("tempGate"))

      println(s"Expression result for tempGate in Anon Scope= ${EvaluateGateExpression("tempGate")}") //Should evaluate (0.9 * 0.2)  + 0.3

      // Testing the expression via a gate
      println(s"Anonymous Scope tempGate = ${TestGate("tempGate", "tempVar")}") // Should return 0.11

      println(s"TestGate result for A in Anon Scope logicGate1 = ${TestGate("logicGate1", "A")}") // Should Return 0.29 since scope of LogicGate1 Exists
    }

    println(s"Global Y after Anon Scope = ${TestGate("global", "Y")}") // Should return 0.4

    // Temp Gate shouldn't exist and SHOULD throw an error
    try {
      println(s"Anonymous Scope tempGate = ${TestGate("tempGate", "tempVar")}")
    } catch {
      case e: Exception =>
        println(s"Error during TestGate for tempGate: ${e.getMessage}")
    }


    // Outside the anonymous scope, the environment should be unchanged
    println(s"TestGate result for A after Anon Scope in logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.5

    println("Running more complex gate evaluations")

    // Example of gate logic assignment with fuzzy expressions
    Scope(Gate("logicGate1")) {
      Assign(Gate("logicGate1"), ADD(MULT(FuzzyVariable("A"), FuzzyValue(0.2)), FuzzyValue(0.3)))(using Gate("logicGate1"))
    }

    println(s"TestGate result for logicGate1 = ${TestGate("logicGate1", "A")}")