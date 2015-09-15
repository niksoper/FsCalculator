module Tests

open System

open Calc

open FsUnit
open NUnit.Framework

[<TestFixture>]
type AccountTest() =

  [<Test>]
  member x.OnePlusOne() = 
    let expectedResult : Result<decimal, Error> = Success 2m
    Calc.tryCalculate "1 1 +" |> should equal expectedResult

  [<Test>]
  member x.TenMinusFive() = 
    let expectedResult : Result<decimal, Error> = Success 5m
    Calc.tryCalculate "10 5 -" |> should equal expectedResult

  [<Test>]
  member x.TenMinusFivePlusFour() = 
    let expectedResult : Result<decimal, Error> = Success 9m
    Calc.tryCalculate "10 5 - 4 +" |> should equal expectedResult

  [<Test>]
  member x.NineTimesNine() = 
    let expectedResult : Result<decimal, Error> = Success 81m
    Calc.tryCalculate "9 9 *" |> should equal expectedResult

  [<Test>]
  member x.TenMod3() = 
    let expectedResult : Result<decimal, Error> = Success 1m
    Calc.tryCalculate "10 3 %" |> should equal expectedResult

  [<Test>]
  member x.TenDiviedByFour() = 
    let expectedResult : Result<decimal, Error> = Success 2.5m
    Calc.tryCalculate "10 4 /" |> should equal expectedResult

  [<Test>]
  member x.NegativesAreSupported() = 
    let expectedResult : Result<decimal, Error> = Success -80m
    Calc.tryCalculate "-1 10 + -9 * -1 -" |> should equal expectedResult

  [<Test>]
  member x.OperatorsAreAppliedFromLeftToRight() = 
    let expectedResult : Result<decimal, Error> = Success 75m
    Calc.tryCalculate "100 50 - 2 / 3 *" |> should equal expectedResult

  [<Test>]
  member x.NumberOnlyReturnsItself() = 
    let expectedResult : Result<decimal, Error> = Success 6m
    Calc.tryCalculate "6" |> should equal expectedResult

  [<Test>]
  member x.NumberOnlyWithWhitespaceReturnsItself() = 
    let expectedResult : Result<decimal, Error> = Success 7m
    Calc.tryCalculate " 7 " |> should equal expectedResult

  [<Test>]
  member x.ExcessWhitespaceInCalculationHasNoEffect() = 
    let expectedResult : Result<decimal, Error> = Success 68m
    Calc.tryCalculate "8   9     + 4 * " |> should equal expectedResult

  [<Test>]
  member x.EmptyInputReturnsNoInputError() = 
    let expectedResult : Result<decimal, Error> = Failure NoInputError
    Calc.tryCalculate "" |> should equal expectedResult

  [<Test>]
  member x.DivisionByZeroReturnsDivideByZeroError() = 
    let expectedResult : Result<decimal, Error> = Failure DivideByZeroError
    Calc.tryCalculate "6 0 /" |> should equal expectedResult

  [<Test>]
  member x.UnsupportedOperatorReturnsUnsupportedOperatorError() = 
    let expectedResult : Result<decimal, Error> = Failure <| UnsupportedOperatorError("|")
    Calc.tryCalculate "6 0 |" |> should equal expectedResult

  [<Test>]
  member x.NoOperandsThrowsExpressionException() = 
    let expectedResult : Result<decimal, Error> = Failure <| ExpressionError
    Calc.tryCalculate "+" |> should equal expectedResult

  [<Test>]
  member x.OneOperandThrowsExpressionException() = 
    let expectedResult : Result<decimal, Error> = Failure <| ExpressionError
    Calc.tryCalculate "7 +" |> should equal expectedResult

  [<Test>]
  member x.ThreeOperandsThrowsExpressionException() = 
    let expectedResult : Result<decimal, Error> = Failure <| ExpressionError
    Calc.tryCalculate "7 6 5 +" |> should equal expectedResult

  [<Test>]
  member x.NoOperatorThrowsExpressionException() = 
    let expectedResult : Result<decimal, Error> = Failure <| ExpressionError
    Calc.tryCalculate "7 6 5" |> should equal expectedResult



