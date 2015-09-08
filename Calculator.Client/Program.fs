open System
open System.Threading

let rec mainLoop() = 
    let input = Console.ReadLine()
    match input with 
    | "q" | "Q" -> ()
    | _ -> 
        try
            let result = Calc.tryCalculate input 
            printfn "%M" result
            mainLoop()
        with
        | ex ->
            printfn "%s" ex.Message
            mainLoop()

[<EntryPoint>]
let main argv = 
    mainLoop()
    printfn "Don't leave me :( "
    Thread.Sleep(750)
    0

