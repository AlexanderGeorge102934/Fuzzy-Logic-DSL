# Fuzzy Logic DSL

## Overview
This project implements a Domain-Specific Language (DSL) for handling fuzzy logic operations. The DSL allows for creating and managing fuzzy variables, applying common fuzzy logic operations (such as AND, OR, ADD, MULT, etc.), and scoping variables both globally and within specific "gates". The gates represent logical contexts in which different operations can be performed. The DSL includes functionality for handling scoping of variables, assigning variables and expressions to gates, and evaluating expressions assigned to gates.

## Key Concepts

1. **Fuzzy Variables and Values**:
   - A **Fuzzy Variable** represents values that can range between 0 and 1. These variables can be assigned specific values using the `Assign` function. Assignments can be global or scoped to a particular gate.
   - A **Fuzzy Value** is a numerical value between 0 and 1 that represents a degree of truth.

2. **Operations**:
   - **Fuzzy Expressions** are operations or transformations applied to fuzzy variables. The DSL supports various fuzzy operations like:
      - **Addition (ADD)**: Sum of two variables, capped at 1.0.
      - **Multiplication (MULT)**: Product of two variables.
      - **Complement (COMPLEMENT)**: Complement of a variable, equivalent to 1 minus the variable's value.
      - **AND (FuzzyAND)**: Takes the minimum of two variables.
      - **OR (FuzzyOR)**: Takes the maximum of two variables.
      - **XOR (FuzzyXOR)**: The absolute difference between two variables.
      - **Alpha Cut (ALPHA_CUT)**: A threshold operation that returns 0 if the variable is below the threshold, otherwise returns the variable's value.

3. **Scopes**:
   - Variables can exist either in a **global scope** or within **local scopes**, referred to as **gates**. Gates represent logical contexts where variables can be defined and fuzzy logic expressions can be assigned and evaluated.

4. **Anonymous Scopes**:
   - Temporary environments that allow you to execute code within a block, ensuring that changes within the block do not persist once the block exits.

5. **Assigning Variables and Expressions**:
   - You can assign fuzzy variables to values and assign fuzzy expressions to gates.

## Key DSL Functions

### 1. **Assigning Variables**:
   - By default, variables are assigned to the **global scope** if not enclosed within a gate-specific `Scope` block.
   - **Syntax for global assignment**: `Assign(FuzzyVariable, FuzzyValue)`
     ```scala
     Assign(FuzzyVariable("X"), FuzzyValue(0.2)) // Global assignment
     ```
   - **Syntax for gate-scoped assignment**: `Scope(Gate){Assign(FuzzyVariable, FuzzyValue)}`
     ```scala
     Scope(Gate("myGate")) {
       Assign(FuzzyVariable("A"), FuzzyValue(0.5)) // A is scoped within myGate
     }
     ```

### 2. **Assigning Expressions to Gates**:
   You can assign a fuzzy expression to a gate, allowing the gate to evaluate the expression.

   - **Syntax**: `Assign(Gate, Expression)(using Gate)`
     ```scala
     Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))
     ```
   - **Gate**: `Gate("logicGate1")` defines the scope where the fuzzy logic expression will be evaluated.
   - **Fuzzy Expression**: The fuzzy logic operation like `ADD(FuzzyVariable("A"), FuzzyVariable("B"))` performs an addition between fuzzy variables A and B (or other operations like `MULT`, `AND`, etc.).
   - **Fuzzy Variables**: Variables (`FuzzyVariable("A")`, `FuzzyVariable("B")`) must be assigned values in the same gate or global scope before assigning an expression to the gate.
   - **`using Gate("logicGate1")`**: This defines the scope for the gate where the fuzzy expression will be evaluated.

### 3. **Testing Variables**:
   - You can use `TestGate` to check the value of a variable within a specific scope.
   - **Syntax for global scope**:
     ```scala
     TestGate("global", "X")
     ```
   - **Syntax for gate-specific scope**:
     ```scala
     TestGate("myGate", "A") // Check value of A within the specific gate "myGate"
     ```

### 4. **Evaluating Gate Expressions**:
   - Once an expression is assigned to a gate, you can evaluate it using `EvaluateGateExpression`.
   - **Syntax**:
     ```scala
     EvaluateGateExpression("logicGate1") // Will return the numeric value of the expression of logicGate1 
     ```

### 5. **Anonymous Scope Usage**:
   - Anonymous scopes allow temporary modifications to the environment. Once the scope is exited, all changes are reverted.
   - **Syntax**:
     ```scala
     AnonymousScope {
       // Temporary assignments and operations
       Scope(Gate("tempGate")) {Assign(FuzzyVariable("tempVar"), FuzzyValue(0.11))}
       Assign(FuzzyVariable("Y"), FuzzyValue(0.9))
     }
     ```
## Error Handling
- **Out-of-scope Variables**: If you attempt to retrieve a variable that has not been assigned or is out of scope, the system will throw an error:
  ```scala
  throw new Exception(s"Variable $name not found globally or in gate $gateName")
  ```
- **Unassigned Gate Expressions**: If you try to evaluate a gate that does not have an assigned expression, the system will throw an error:
  ```scala
  throw new Exception(s"No expression assigned to gate $gateName")
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
Clone the project to your local machine:
