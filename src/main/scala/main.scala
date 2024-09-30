import scala.collection.mutable

type Environment = mutable.Map[String, mutable.Map[String, Double]] // Map from gate name to variables
type GlobalEnvironment = mutable.Map[String, Double] // Global variable environment

object FuzzyLogicDSL:

  given env: Environment = mutable.Map() // Environment for storing variables within specific gates
  val globalEnv: GlobalEnvironment = mutable.Map() // global environment for variables not tied to a specific gate

  // Abstract class for fuzzy expressions, with the 'eval' method for evaluation
  abstract class FuzzyExpr:
    def eval: Double


  case class Gate(name: String) // Define a gate by name
  case class GateSystem(gates: mutable.Map[String, FuzzyExpr] = mutable.Map()) // Holds all gates and their associated fuzzy expressions
  val gateSystem = GateSystem()

  // The global gate system for storing gates and their expressions
  case class FuzzyValue(v: Double) extends FuzzyExpr:
    def eval: Double = v




  case class FuzzyVariable(name: String) extends FuzzyExpr:// Represents a fuzzy value that can be evaluated
    def eval: Double =
      // Try fetching the value from the environment based on the current gate's scope
      summon[Environment].collectFirst {

        case (_, scope) if scope.contains(name) => scope(name)
      }.getOrElse(globalEnv.getOrElse(name, throw new Exception(s"Variable $name not found in any scope or globally")))
        // If not found in scope env look at global scope if nothing there then throw exception



  // Fuzzy Operations
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


  // Assign variable to local/global scope or gate to expression
  def Assign(left: Any, right: FuzzyExpr)(using gate: Gate = Gate("global")): Unit = left match

    case FuzzyVariable(name) =>
      if gate.name == "global" then // If global assign global scope
        globalEnv.update(name, right.eval)

      else
        summon[Environment].getOrElseUpdate(gate.name, mutable.Map[String, Double]()).update(name, right.eval)

    // Assign a fuzzy expression to a Gate (store it in the gate system)
    case gate: Gate => gateSystem.gates(gate.name) = right
    case _ => throw new Exception("Invalid assignment")


  // Define a scope for a specific gate, allowing assignments and operations within the gate
  def Scope(gate: Gate)(block: Gate ?=> Unit): Unit =
    given Gate = gate
    block

  // Define an anonymous scope that temporarily modifies variables and restores them after execution
  def AnonymousScope(block: => Unit): Unit =
    // Save current environment specific gates
    val originalEnv = summon[Environment].map {
      case (k, v) => k -> v.clone()
    }
    val originalGlobalEnv = globalEnv.clone()


    try {
      block
    } finally {
      // Restore environments and globals
      summon[Environment].clear()
      summon[Environment].addAll(originalEnv)
      globalEnv.clear()
      globalEnv.addAll(originalGlobalEnv)
    }


  // Test the logic gate result using the active bindings in the gate's context or globally
  def TestGate(gateName: String, variableName: String): Double =
    // Check if a gate exists and the variable is scoped within that gate
    if summon[Environment].contains(gateName) then
      summon[Environment](gateName).get(variableName) match {
        case Some(value) => value

        // Look in global scope if not in local
        case None => globalEnv.getOrElse(variableName, throw new Exception(s"Variable $variableName not found globally or in gate $gateName"))
      }

    else
      // If no gate look at global
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

    // Assign global variables X and Y
    Assign(FuzzyVariable("X"), FuzzyValue(0.2)) // Global assignment
    Assign(FuzzyVariable("Y"), FuzzyValue(0.4)) // Global assignment

    // Test global assignments
    println(s"Global X = ${TestGate("global", "X")}") // Should return 0.2
    println(s"Global Y = ${TestGate("global", "Y")}") // Should return 0.4

    // Assign variables A and B within a scope (logicGate1)
    Scope(Gate("logicGate1")) {
      Assign(FuzzyVariable("A"), FuzzyValue(0.5))
      Assign(FuzzyVariable("B"), FuzzyValue(0.7))
    }

    // Assign an expression to a gate (logicGate1) and evaluate it
    Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))

    // Test the results of logicGate1
    println(s"TestGate result for A in logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.5
    println(s"TestGate result for Global X in logicGate1 = ${TestGate("logicGate1", "X")}") // Should return 0.2
    println(s"Expression result for logicGate1 A + B = ${EvaluateGateExpression("logicGate1")}") // Should evaluate A + B = 0.5 + 0.7

    // Change the value of X locally in logicGate1
    Scope(Gate("logicGate1")) {
      Assign(FuzzyVariable("X"), FuzzyValue(0.7))
    }

    // Test the local and global values of X
    println(s"TestGate result for local X in logicGate1 = ${TestGate("logicGate1", "X")}") // Should return 0.7
    println(s"TestGate result for Global X after locally changing local X = ${TestGate("global", "X")}") // Should return 0.2

    // Using an anonymous scope to temporarily change values
    AnonymousScope {
      Assign(FuzzyVariable("Y"), FuzzyValue(0.90)) // Global assignment within anon scope
      println(s"Global Y in Anon Scope = ${TestGate("global", "Y")}") // Should return 0.9

      // Assign a temporary variable in tempGate
      Scope(Gate("tempGate")) {
        Assign(FuzzyVariable("tempVar"), FuzzyValue(0.11)) // Assign a value to tempVar
      }

      // Temporarily modify A in logicGate1 within anon scope
      Scope(Gate("logicGate1")) {
        Assign(FuzzyVariable("A"), FuzzyValue(0.29))
      }

      // Test logicGate1 with modified A
      println(s"Expression result for logicGate1 A + B (A is now 0.29, Anon Scope) = ${EvaluateGateExpression("logicGate1")}") // Should evaluate A + B = 0.29 + 0.7

      // Evaluate an expression in tempGate
      Assign(Gate("tempGate"), ADD(MULT(FuzzyValue(0.9), FuzzyValue(0.2)), FuzzyValue(0.3)))(using Gate("tempGate"))
      println(s"Expression result for tempGate in Anon Scope = ${EvaluateGateExpression("tempGate")}") // Should evaluate (0.9 * 0.2) + 0.3

      // Test the temporary variable in tempGate
      println(s"Anonymous Scope tempGate = ${TestGate("tempGate", "tempVar")}") // Should return 0.11

      // Test the modified A in logicGate1 within the anon scope
      println(s"TestGate result for A in Anon Scope logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.29 since scope of logicGate1 exists
    }

    // Test Y after exiting the anonymous scope
    println(s"Global Y after Anon Scope = ${TestGate("global", "Y")}") // Should return 0.4

    // Test tempGate after exiting the anonymous scope (shouldn't exist) and SHOULD throw an error
    try {
      println(s"Anonymous Scope tempGate = ${TestGate("tempGate", "tempVar")}")
    } catch {
      case e: Exception =>
        println(s"Error during TestGate for tempGate: ${e.getMessage}")
    }

    // Test logicGate1 after exiting the anonymous scope (A should revert)
    println(s"TestGate result for A after Anon Scope in logicGate1 = ${TestGate("logicGate1", "A")}") // Should return 0.5

