#r "../packages/FSPowerPack.Core.Community.2.0.0.0/Lib/Net40/FSharp.PowerPack.dll"
#load "TypeDefinitions.fs"
#load "SymbolicComputation.fs"

open SymbolicComputation
open System

let rec evalString = function
| "quit" -> true
| expressionString -> 
    try
        expressionString
        |> Expression.fromString
        |> printfn "%O"
    with
        | e -> printfn "%s" e.Message
    Console.ReadLine()
    |> evalString
     
Console.ReadLine() 
|> evalString 