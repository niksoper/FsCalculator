module Tests

open System

open Calc

open FsUnit
open NUnit.Framework

let expectException exceptionType input =
    (fun () -> Calc.tryCalculate input |> ignore) |> should throw exceptionType

[<TestFixture>]
type AccountTest() =

  [<Test>]
  member x.OnePlusOne() = 
    Calc.tryCalculate "1 1 +" |> should equal 2

  [<Test>]
  member x.TenMinusFive() = 
    Calc.tryCalculate "10 5 -" |> should equal 5

  [<Test>]
  member x.TenMinusFivePlusFour() = 
    Calc.tryCalculate "10 5 - 4 +" |> should equal 9

  [<Test>]
  member x.NineTimesNine() = 
    Calc.tryCalculate "9 9 *" |> should equal 81

  [<Test>]
  member x.TenMod3() = 
    Calc.tryCalculate "10 3 %" |> should equal 1

  [<Test>]
  member x.TenDiviedByFour() = 
    Calc.tryCalculate "10 4 /" |> should equal 2.5m

  [<Test>]
  member x.NegativesAreSupported() = 
    Calc.tryCalculate "-1 10 + -9 * -1 -" |> should equal -80

  [<Test>]
  member x.OperatorsAreAppliedFromLeftToRight() = 
    Calc.tryCalculate "100 50 - 2 / 3 *" |> should equal 75

  [<Test>]
  member x.NumberOnlyReturnsItself() = 
    Calc.tryCalculate "6" |> should equal 6

  [<Test>]
  member x.NumberOnlyWithWhitespaceReturnsItself() = 
    Calc.tryCalculate " 7 " |> should equal 7

  [<Test>]
  member x.ExcessWhitespaceInCalculationHasNoEffect() = 
    Calc.tryCalculate "8   9     + 4 * " |> should equal 68

  [<Test>]
  member x.EmptyInputThrowsNoInputException() = 
    "" |> expectException typeof<NoInputException>

  [<Test>]
  member x.DivisionByZeroThrowsDivideByZeroException() = 
    "6 0 /" |> expectException typeof<DivideByZeroException>

  [<Test>]
  member x.UnsupportedOperatorThrowsUnsupportedOperatorException() = 
    "6 0 |" |> expectException typeof<UnsupportedOperatorException>

  [<Test>]
  member x.NoOperandsThrowsOperandException() = 
    "+" |> expectException typeof<ExpressionException>

  [<Test>]
  member x.OneOperandThrowsOperandException() = 
    "7 +" |> expectException typeof<ExpressionException>

  [<Test>]
  member x.ThreeOperandsThrowsOperandException() = 
    "7 6 5 +" |> expectException typeof<ExpressionException>

  [<Test>]
  member x.NoOperatorThrowsOperandException() = 
    "7 6 5" |> expectException typeof<ExpressionException>



