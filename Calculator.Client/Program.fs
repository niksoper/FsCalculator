open System
open System.Threading

open Calc

let intro() = 
    printfn "Stack Based Calculator"
    printfn ""
    printfn "Supported operators are: %s" Calc.supportedOperators

let errorMessage failure = 
    match failure with
    | NoInputError -> "No input was provided"
    | ExpressionError -> "Each operation requires an operator and exactly two operands."
    | UnsupportedOperatorError -> "Unrecognised operator"

let rec mainLoop() = 
    let input = Console.ReadLine()
    match input with 
    | "q" | "Q" -> ()
    | _ -> 
        try
            let result = Calc.tryCalculate input 
            match result with
            | Calc.Success s    -> printfn "%M" s
            | Calc.Failure fail -> printfn "Error: %s" <| errorMessage fail
            mainLoop()
        with
        | ex ->
            printfn "Exception: %s" ex.Message
            mainLoop()

[<EntryPoint>]
let main argv = 
    intro()
    mainLoop()
    printfn "Don't leave me :( "
    Thread.Sleep(750)
    0

