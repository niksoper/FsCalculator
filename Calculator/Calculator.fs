module Calc

open System

type Error = 
| NoInputError
| ExpressionError
| UnsupportedOperatorError of string
| DivideByZeroError

type Result<'S, 'F> =
| Success of 'S
| Failure of 'F

let bind (f : 'S -> Result<'S2,'F2>) m=
    match m with 
    | Success s -> f s
    | Failure f -> Failure f

let (>>=) m f = bind f m 

type CalculationPart =
| Number of decimal
| Operation of (decimal -> decimal -> decimal)

let operations = 
    [ "+", (+)
      "-", (-)
      "*", (*)
      "/", (/)
      "%", (%) ]

let (|StrOp|_|) str = 
    operations
    |> Map.ofList
    |> Map.tryFind str

let (|StrNum|_|) str =
    match Decimal.TryParse(str) with
    | (true , value) -> Some(value)
    | _ -> None

let parsePart part =
    match part with
    | StrNum n      -> Success <| Number(n)
    | StrOp o       -> Success <| Operation(o)
    | unrecognised  -> Failure <| UnsupportedOperatorError(unrecognised)

let popResult stack = 
    match stack with
    | Number a :: Number b :: Operation f :: rest -> 
        let result = Number(f a b)
        Success(result, rest) 
    | _ -> Failure ExpressionError

let push part stack = part :: stack

let mergeIfAllSuccess (input : Result<'S,'F> list) : Result<'S list, 'F> =
    let rec foldFunc (results : Result<'S,'F> list) (acc : Result<'S list, 'F>) : Result<'S list, 'F> = 
        match results with
        | head :: tail -> head >>= (fun s -> acc >>= (fun list -> foldFunc tail (Success <| s::list)))
        | [] -> acc 
    Success []
    |> foldFunc input
    >>= (fun s -> Success <| List.rev s)

let passStackIfValid (input : string list) : Result<string list, Error> = 
    let dividingByZero = 
        input
        |> List.pairwise
        |> List.tryFind (function | ("0", "/") -> true | _ -> false )
    match dividingByZero with
    | Some _ -> Failure DivideByZeroError
    | None -> Success input

let parseStack (input : string) = 
    input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
    |> Array.toList
    |> passStackIfValid
    >>= (fun s -> 
        s
        |> List.map parsePart
        |> mergeIfAllSuccess)        

let rec calculate stack =
    match stack with
    | [] -> Failure NoInputError
    | [Number n] -> Success n
    | _ ->
        stack
        |> popResult
        >>= (fun (result, stack') -> 
            stack' 
            |> push result 
            |> calculate)

let tryCalculate input = 
    input 
    |> parseStack
    >>= calculate
    
let supportedOperators = 
    let symbols = operations |> List.map (fun (c,f) -> c)
    String.Join(" ", symbols)