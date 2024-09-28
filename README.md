# Domain Specific Language FuzzyLogic

## Overview
This project implements a DSL for fuzzy logic operations. It allows the creation and evaluation of fuzzy expressions (such as AND, OR, XOR) and supports scoping of variables both globally and within specific gates. 

## Features 
Fuzzy Expressions: Supports various fuzzy logic operations such as addition, multiplication, complement, AND, OR, XOR, and alpha cuts.
Scoping: Variables can be scoped within specific logic gates or globally.
Anonymous Scopes: Allows temporary changes to the environment that revert back to their original state after execution.
Gate Evaluation: Fuzzy expressions can be assigned to gates and evaluated.

# DSL Syntax
## Fuzzy Expressions
You can create fuzzy expressions using predefined operations such as ADD, MULT, AND, OR, XOR, COMPLEMENT, and ALPHA_CUT. These expressions work on fuzzy variables and values within specific gates or globally.

### Example Operations:
''' val expr1 = ADD(FuzzyValue(0.5), FuzzyValue(0.4))    // Returns min(1.0, 0.5 + 0.4)
val expr2 = AND(FuzzyVariable("A"), FuzzyVariable("B")) // Returns min(A, B)
val expr3 = COMPLEMENT(FuzzyVariable("A")) // Returns 1.0 - A '''




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




