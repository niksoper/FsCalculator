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

let bind func m=
    match m with 
    | Success s -> func s
    | Failure f -> Failure f

let (>>=) m f = bind f m 

//TODO correct name
let raiseOne (func: 'a -> 'b) a= 
    Success (func a)

let raiseTwo (func: 'a -> 'b -> 'c) a b = 
    Success (func a b)

type ResultBuilder() = 
    member x.Bind(v,f) = bind f v
    member x.Return v = Success v
    member x.ReturnFrom r = r

let result = new ResultBuilder()

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

let calculateHead stack = 
    match stack with
    | Number a :: Number b :: Operation f :: rest -> 
        let result = Number(f a b)
        Success (result :: rest)
    | _ -> Failure ExpressionError

let concat a b = a::b

let raisedConcat a b= (raiseTwo concat) a b

let mergeIfAllSuccess (input : Result<'S,'F> list) : Result<'S list, 'F> =
    let rec mergeFunc (results : Result<'S,'F> list) (acc : Result<'S list, 'F>) : Result<'S list, 'F> = 
        match results with
        | head :: tail -> 
            head 
            >>= (fun x -> acc >>= (fun list -> Success <| x::list))
            |> (mergeFunc tail)
        | [] -> acc 
    mergeFunc input (Success [])
    >>= (raiseOne <| List.rev)

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
    >>= (raiseOne <| List.map parsePart)
    >>= mergeIfAllSuccess       

let rec calculate stack =
    match stack with
    | [] -> Failure NoInputError
    | [Number n] -> Success n
    | _ ->
        stack
        |> calculateHead
        >>= calculate

let tryCalculate input = 
    result {
        let! calcList = parseStack input
        return! calculate calcList
    }
    
let supportedOperators = 
    let symbols = operations |> List.map (fun (c,f) -> c)
    String.Join(" ", symbols)