#r "../packages/FSPowerPack.Core.Community.2.0.0.0/Lib/Net40/FSharp.PowerPack.dll"
#load "TypeDefinitions.fs"
#load "SymbolicComputation.fs"

#r "../packages/Unquote.3.1.2/lib/net45/Unquote.dll"

open SymbolicComputation
open System
open Swensen.Unquote.Assertions

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