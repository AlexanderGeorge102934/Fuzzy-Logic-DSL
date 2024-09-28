# Fuzzy Logic DSL

## Overview
This project implements a Domain-Specific Language (DSL) for handling fuzzy logic operations. The DSL allows for creating and managing fuzzy variables, applying common fuzzy logic operations (such as AND, OR, ADD, MULT, etc.), and scoping variables both globally and within specific "gates". The gates represent logical contexts in which different operations can be performed. The DSL includes functionality for handling scoping of variables, assigning variables and expressions to gates, and evaluating expressions assigned to gates.

## Key Concepts

1. **Fuzzy Variables and Values**:
   - A **Fuzzy Variable** represents a named entity with a fuzzy value between 0 and 1.
   - A **Fuzzy Value** is a numerical value between 0 and 1 that represents a degree of truth.
   
2. **Operations**:
   - You can perform several fuzzy operations, such as addition (capped at 1), multiplication, AND (min), OR (max), XOR, complement, and alpha cuts.
   
3. **Scopes**:
   - Variables can exist either in a **global scope** or within **local scopes**, referred to as **gates**. Gates represent logical contexts where variables can be defined and fuzzy logic expressions can be assigned and evaluated.
   
4. **Anonymous Scopes**:
   - Temporary environments that allow you to execute code within a block, ensuring that changes within the block do not persist once the block exits.

5. **Assigning Variables and Expressions**:
   - You can assign fuzzy variables to values and assign fuzzy expressions to gates.

## Key DSL Functions

1. **Assigning Variables**:
   - By default, variables are assigned to the **global scope** if not enclosed within a gate-specific `Scope` block.
   - Syntax for global assignment:
     ```scala
     Assign(FuzzyVariable("X"), FuzzyValue(0.2)) // Global assignment
     ```
   - Syntax for gate-scoped assignment:
     ```scala
     Scope(Gate("myGate")) {
       Assign(FuzzyVariable("A"), FuzzyValue(0.5)) // A is scoped within myGate
     }
     ```
   - In the above example, `A` is scoped within `myGate`.

2. **Assigning Expressions to Gates**:
   - You can assign a fuzzy expression to a gate, allowing the gate to evaluate the expression.
   - Syntax:
     ```scala
     Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))
     ```

3. **Testing Variables**:
   - You can use `TestGate` to check the value of a variable within a specific scope.
   - Syntax for global scope:
     ```scala
     TestGate("global", "X")
     ```
   - Syntax for gate-specific scope:
     ```scala
     TestGate("myGate", "A")
     ```

4. **Evaluating Gate Expressions**:
   - Once an expression is assigned to a gate, you can evaluate it using `EvaluateGateExpression`.
   - Syntax:
     ```scala
     EvaluateGateExpression("logicGate1")
     ```

5. **Anonymous Scope Usage**:
   - Anonymous scopes allow temporary modifications to the environment. Once the scope is exited, all changes are reverted.
   - Syntax:
     ```scala
     AnonymousScope {
       // Temporary assignments and operations
       Assign(FuzzyVariable("Y"), FuzzyValue(0.9))
     }
     ```




## Project Structure
- `src/main/scala/main.scala` – Main code for the project.
- `src/test/scala/FuzzyLogicTest.scala` – Test cases for fuzzy logic operations.
- `build.sbt` – SBT configuration file for building and managing dependencies.

## Getting Started

### Prerequisites
To get started with this project, you'll need:
- Scala 2.13.x or higher
- SBT (Simple Build Tool) installed
- IntelliJ IDEA or another Scala-compatible IDE

### Cloning the repository
Clone the project to your local machine




