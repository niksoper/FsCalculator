module Calc

open System

type Error = 
| NoInputError
| ExpressionError
| UnsupportedOperatorError

type NoInputException()                 = inherit ApplicationException("No input was provided.")
type ExpressionException()              = inherit ApplicationException("Each operation requires an operator and exactly two operands.")
type UnsupportedOperatorException(op)   = inherit ApplicationException(sprintf "Unrecognised operator: %s" op)

type Result<'S, 'F> =
| Success of 'S
| Failure of 'F

let bind (f : 'S -> Result<'S,'F>)  m =
    match m with 
    | Success s -> f s
    | Failure f -> m

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
    | unrecognised  -> Failure UnsupportedOperatorError

let pop stack =
    match stack with
    | top :: rest ->
        let newStack = rest
        (top, newStack)
    | [] -> raise (ExpressionException())

let push part stack = part :: stack

//let combineIfAllSuccesses (results : Result<'S,'F> list) = 
//    let anyFailure = results |> List.tryFind ( fun x -> match x with | Success _ -> false | Failure _ -> true)
//    match anyFailure with
//    | Some failure -> failure
//    | None -> 

let combineIfAllSuccesses' (results : Result<'S,'F> list) = 
    let successes, failures = results |> List.partition ( fun x -> match x with | Success _ -> false | Failure _ -> true)
    match failures with
    | fail :: _ -> fail
    | _ -> successes

let parseStack (input : string) = 
    let parseResult = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map parsePart

    
    //|> Array.toList

let rec calculate stack =
    match stack with
    | [] -> raise (NoInputException())
    | [Number n] -> n
    | _ ->
        let a, stack' = pop stack
        let b, stack'' = pop stack'
        let f, stack''' = pop stack''
        match (a,b,f) with
        | Number a', Number b', Operation f' -> 
            let result = Number(f' a' b')
            push result stack''' |> calculate
        | _ -> raise (ExpressionException())

let tryCalculate input = 
    input
    |> parseStack
    >>= calculate 

let supportedOperators = 
    let symbols = operations |> List.map (fun (c,f) -> c)
    String.Join(" ", symbols)