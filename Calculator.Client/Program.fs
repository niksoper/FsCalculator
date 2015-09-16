open System
open System.Threading

open Calc

let intro() = 
    printfn "Stack Based Calculator"
    printfn ""
    printfn "Supported operators are: %s" Calc.supportedOperators

let errorMessage failure = 
    match failure with
    | NoInputError -> "No input was provided."
    | ExpressionError -> "Each operation requires an operator and exactly two operands."
    | UnsupportedOperatorError op -> sprintf "Unrecognised operator: %s" op
    | DivideByZeroError -> "Cannot divide by zero. Try another universe."

let rec mainLoop() = 
    let input = Console.ReadLine()
    match input with 
    | "q" | "Q" -> ()
    | _ -> 
        let result = Calc.tryCalculate input 
        match result with
        | Calc.Success s    -> printfn "%M" s
        | Calc.Failure fail -> printfn "Error: %s" <| errorMessage fail
        mainLoop()

[<EntryPoint>]
let main argv = 
    intro()
    mainLoop()
    printfn "Don't leave me :( "
    Thread.Sleep(750)
    0

