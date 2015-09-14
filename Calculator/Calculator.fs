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
    | StrNum n      -> Number(n)
    | StrOp o       -> Operation(o)
    | unrecognised  -> raise (UnsupportedOperatorException(unrecognised))

let pop stack =
    match stack with
    | top :: rest ->
        let newStack = rest
        (top, newStack)
    | [] -> raise (ExpressionException())

let push part stack = part :: stack

let parseStack (input : string) = 
    input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
    |> Array.map parsePart
    |> Array.toList

let rec calculate stack : Result<decimal, Error> =
    match stack with
    | [] -> Failure NoInputError
    | [Number n] -> Success n
    | _ ->
        let a, stack' = pop stack
        let b, stack'' = pop stack'
        let f, stack''' = pop stack''
        match (a,b,f) with
        | Number a', Number b', Operation f' -> 
            let result = Number(f' a' b')
            push result stack''' |> calculate
        | _ -> Failure ExpressionError

let tryCalculate input = 
    input
    |> parseStack
    |> calculate 

let supportedOperators = 
    let symbols = operations |> List.map (fun (c,f) -> c)
    String.Join(" ", symbols)