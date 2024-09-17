case class FuzzySet[A](elements: Map[A, Double]) {

  // Fuzzy Union (OR)
  def union(otherSet: FuzzySet[A]): FuzzySet[A] = {
    FuzzySet(elements.map {
      case (x, valueA) =>
      val valueB = otherSet.elements.getOrElse(x, 0.0) // If x is not in B, treat as 0
      (x, math.max(valueA, valueB)) // Union takes the max value
    })
  }

  // Fuzzy Intersection (AND)
  def intersection(otherSet: FuzzySet[A]): FuzzySet[A] = {
    FuzzySet(elements.map {
      case (x, valueA) =>
        val valueB = otherSet.elements.getOrElse(x, 0.0) // If x is not in B, treat as 0
        (x, math.min(valueA, valueB)) // Union takes the max value
    })
  }
  
  
}
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
