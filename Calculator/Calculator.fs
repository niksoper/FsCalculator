module Calc

open System

type NoInputException()                 = inherit ApplicationException("No input was provided.")
type ExpressionException()              = inherit ApplicationException("Each operation requires an operator and exactly two operands.")
type UnsupportedOperatorException(op)   = inherit ApplicationException(sprintf "Unrecognised operator: %s" op)

type CalculationPart =
| Number of decimal
| Operation of (decimal -> decimal -> decimal)

let (|StrNum|_|) str =
    match Decimal.TryParse(str) with
    | (true , value) -> Some(value)
    | _ -> None

let (|StrOp|_|) str =
    match str with
    | "+" -> Some(+)
    | "-" -> Some(-)
    | "*" -> Some(*)
    | "/" -> Some(/)
    | "%" -> Some(%)
    | _ -> None

let parsePart part =
    match part with
    | StrNum n         -> Number(n)
    | StrOp o          -> Operation(o)
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

let rec calculate (stack : CalculationPart list) =
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
    |> calculate 