#if INTERACTIVE
#r "../packages/FSPowerPack.Core.Community.3.0.0.0/Lib/Net40/FSharp.PowerPack.dll"
#load "TypeDefinitions.fs"
#load "SymbolicComputation.fs"
#r "../packages/Unquote.3.1.2/lib/net45/Unquote.dll"
#else
module Test
#endif

open SymbolicComputation
open TypeDefinitions
open Swensen.Unquote.Assertions
open Swensen.Unquote.Extensions
open Swensen.Unquote.Operators

let testWhiteSpace () = 
    <@ "   4        \t" |> Expression.fromString |> string = "4" @> |> test
    <@ "   2323      \r\t" |> Expression.fromString |> string = "2323" @> |> test

let testUnaryOperators () = 
    <@ "-2" |> Expression.fromString |> string = "-2" @> |> test
    <@ "+14" |> Expression.fromString |> string = "14" @> |> test
    <@ "+e" |> Expression.fromString |> string = "(e)" @> |> test
    <@ "-x" |> Expression.fromString |> string = "-(x)" @> |> test
    <@ "-+1" |> Expression.fromString |> string = "-1" @> |> test
    <@ "+-1" |> Expression.fromString |> string = "-1" @> |> test

let testBinaryOperators () = 
    <@ "2 + 2" |> Expression.fromString |> string = "4" @> |> test
    <@ "6 * 2" |> Expression.fromString |> string = "12" @> |> test
    <@ "2 - 3" |> Expression.fromString |> string = "-1" @> |> test
    <@ "12 / 18" |> Expression.fromString |> string = "2/3" @> |> test

let testPrecedence () = 
    <@ "5-2 * 3 + -4" |> Expression.fromString |> string = "-5" @> |> test
    <@ "2 * 3 + 1" |> Expression.fromString |> string = "7" @> |> test
    <@ "8^ 2 * 3 - 6 * 8/2" |> Expression.fromString |> string = "168" @> |> test
    <@ "3^3^2 * 4" |> Expression.fromString |> string = "78732" @> |> test
    <@ "3^-2" |> Expression.fromString |> string = "1/9" @> |> test
    <@ "1/2/3/4/5/6/7/8/9/10" |> Expression.fromString |> string = "1/3628800" @> |> test
    <@ "1/1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10" |> Expression.fromString |> string = "7381/2520" @> |> test
    <@ "1/1-1/2+1/4-1/8+1/16-1/32" |> Expression.fromString |> string = "21/32" @> |> test

let testParentheses () = 
    <@ "1 + (2 * 3)" |> Expression.fromString |> string = "7" @> |> test
    <@ "(1 + 2 ) * 3" |> Expression.fromString |> string = "9" @> |> test

// test all tests
try 
    testWhiteSpace()
    testUnaryOperators()
    testBinaryOperators()
    testPrecedence()
    testParentheses()
    printfn "All test cases passed!"
with
    | e -> printfn "%s" e.Message