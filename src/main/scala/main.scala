case class FuzzySet[A](elements: Map[A, Double]) {

  // Perform fuzzy set operation based on input string
  def performOperation(operation: String, otherSet: Option[FuzzySet[A]] = None): FuzzySet[A] = {
    operation match {
      // Fuzzy Union (OR)
      case "union" =>
        otherSet match {
          case Some(set) =>
            FuzzySet(elements.map { case (x, valueA) =>
              val valueB = set.elements.getOrElse(x, 0.0)
              (x, math.max(valueA, valueB)) // Union: take the max value
            })
          case None => throw new IllegalArgumentException("Union requires another set")
        }

      // Fuzzy Intersection (AND)
      case "intersection" =>
        otherSet match {
          case Some(set) =>
            FuzzySet(elements.map { case (x, valueA) =>
              val valueB = set.elements.getOrElse(x, 0.0)
              (x, math.min(valueA, valueB)) // Intersection: take the min value
            })
          case None => throw new IllegalArgumentException("Intersection requires another set")
        }

      // Fuzzy Complement (NOT)
      case "complement" =>
        FuzzySet(elements.map { case (x, value) =>
          (x, 1.0 - value) // Complement: 1 - value
        })

      // Fuzzy Addition (capped at 1)
      case "add" =>
        otherSet match {
          case Some(set) =>
            FuzzySet(elements.map { case (x, valueA) =>
              val valueB = set.elements.getOrElse(x, 0.0)
              (x, math.min(1.0, valueA + valueB)) // Add and cap at 1.0
            })
          case None => throw new IllegalArgumentException("Add requires another set")
        }

      // Fuzzy Multiplication
      case "multi" =>
        otherSet match {
          case Some(set) =>
            FuzzySet(elements.map { case (x, valueA) =>
              val valueB = set.elements.getOrElse(x, 0.0)
              (x, valueA * valueB) // Multiply values
            })
          case None => throw new IllegalArgumentException("Multiplication requires another set")
        }

      // Default case for unknown operation
      case _ => throw new IllegalArgumentException(s"Unknown operation: $operation")
    }
  }

  // alphaCut function that returns a set of elements whose membership is >= alpha
  def alphaCut(alpha: Double): Set[A] = {
    elements.filter { case (_, membership) => membership >= alpha }.keys.toSet
  }
}




// GateSystem class for managing logic gates
case class GateSystem(gates: Map[String, FuzzySet[String]] = Map()) {

  // Assign a gate to a name, return a new instance of GateSystem with updated gates
  def assign(name: String, result: FuzzySet[String]): GateSystem = {
    GateSystem(gates + (name -> result)) // Return a new GateSystem with updated gates
  }

  // Test the logic gate result for a particular element
  def testGate(gateName: String, inputName: String): Option[Double] = {
    gates.get(gateName).flatMap(_.elements.get(inputName)) // Return the element's value if found
  }
}

// Operation functions that return FuzzySets
//def ADD(setA: FuzzySet[String], setB: FuzzySet[String]): FuzzySet[String] = setA.add(setB)
//def MULTI(setA: FuzzySet[String], setB: FuzzySet[String]): FuzzySet[String] = setA.multi(setB)
//def OR(setA: FuzzySet[String], setB: FuzzySet[String]): FuzzySet[String] = setA.union(setB)
//def AND(setA: FuzzySet[String], setB: FuzzySet[String]): FuzzySet[String] = setA.intersection(setB)
//def NOT(setA: FuzzySet[String]): FuzzySet[String] = setA.complement
  //TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  //TIP Press <shortcut actionId="ShowIntentionActions"/> with your caret at the highlighted text
  // to see how IntelliJ IDEA suggests fixing it.
  (1 to 5).map(println)

  for (i <- 1 to 5) {
    //TIP Press <shortcut actionId="Debug"/> to start debugging your code. We have set one <icon src="AllIcons.Debugger.Db_set_breakpoint"/> breakpoint
    // for you, but you can always add more by pressing <shortcut actionId="ToggleLineBreakpoint"/>.
    println(s"i = $i")
  }
