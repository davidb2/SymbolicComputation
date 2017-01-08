module SymbolicComputation

open TypeDefinitions
open System
open System.Numerics
open FSharp.Core

let private MAX_PREC = operator.Pow.Precedence
/// Maximum integer
let private MAX_INT_BIGNUM = bignum.FromInt ((1 <<< 31) - 1) 

/// Converts number to expression.Num
let private toNum n = Num(RegularConstant(Rational(n)))

/// Converts expression.Num to number
let private fromNum = function
| Num(RegularConstant(Rational(n))) -> n
| _ -> failwith "not number."

/// Determines if the argument is a Regular Constant
let private isNum = function
| Num(RegularConstant(_)) -> true
| _ -> false

/// Defines and holds functions that operate on expressions
module Expression =
    /// The number 0.
    let ZERO = toNum 0N

    /// The number 1.
    let ONE = toNum 1N

    /// Characters that are reserved for special constants
    let reservedConstants = Set.ofList [PI; E]
    let allowedVariables = 
        ['a'..'d'] @ ['f'..'h'] @ ['j'..'z'] @ ['A'..'Z']
        |> Set.ofList

    /// Make sure expression contains only one variable
    let private makeSureExpressionIsMonoVariate stringExpression =
        let variables = 
            stringExpression
            |> List.filter (fun c -> allowedVariables |> Set.contains c)
            |> List.distinct
            |> List.length
        match variables with
        | 0 | 1 -> stringExpression
        | _ -> failwith "This module only works with single variable expressions."

    /// Determines if character is a whitespace character
    let private isWhiteSpace = 
        let whiteSpaceCharacters = Set.ofList [' '; '\t'; '\r']
        fun character ->
            whiteSpaceCharacters
            |> Set.exists (fun c -> c = character) 

    /// Determines if character is an operator character
    let private isOperator =
        let operators = Set.ofList ['+'; '-'; '*'; '/'; '^'; '=']
        fun operator ->
            operators
            |> Set.exists (fun o -> o = operator)

    /// Determines if character is a prefix character
    let private isPrefix =
        let prefixes = Set.ofList ['+'; '-']
        fun prefix ->
            prefixes
            |> Set.exists (fun p -> p = prefix)
    
    /// Returns the first number of the string
    let private getFirstNumber stringExpr =
        let splitIndex = 
            stringExpr 
            |> List.tryFindIndex (fun c -> not ('0' <= c && c <= '9')) 
        match splitIndex with
        | None -> 
            stringExpr 
            |> String.Concat
            |> bignum.Parse, []
        | Some(index) -> 
            let num, rest = 
                stringExpr
                |> List.splitAt index 
            num 
            |> List.toArray 
            |> String.Concat
            |> bignum.Parse, rest
    
    /// Simplifies the given expression
    let rec private simplify unsimplifiedExpression = 
        let innerSimplifiedExpression = 
            match unsimplifiedExpression with
            | Num(n) -> Num(n)
            | Var(v) -> Var(v)
            | Neg(n) -> Neg(simplify n)
            | Pos(n) -> Pos(simplify n)
            | Add(x, y) -> Add(simplify x, simplify y)
            | Sub(x, y) -> Sub(simplify x, simplify y)
            | Mul(x, y) -> Mul(simplify x, simplify y)
            | Div(x, y) -> Div(simplify x, simplify y)
            | Pow(x, y) -> Pow(simplify x, simplify y)

        match innerSimplifiedExpression with
        | Neg(Num(RegularConstant(Rational(n)))) -> toNum (- n) 
        | Pos(Num(RegularConstant(Rational(n)))) -> toNum (+ n)
        | Add(x, y) as e ->
            match x, y with
            | a, b when isNum a && isNum b -> (fromNum a) + (fromNum b) |> toNum
            | a, b when a = ZERO -> b
            | a, b when b = ZERO -> a 
            | _ -> e
        | Sub(x, y) as e ->
            match x, y with
            | a, b when isNum a && isNum b -> (fromNum a) - (fromNum b) |> toNum
            | a, b when b = ZERO -> a |> simplify
            | a, b when a = ZERO -> Neg(b) |> simplify
            | _ -> e
        | Mul(x, y) as e -> 
            match x, y with
            | a, b when isNum a && isNum b -> (fromNum a) * (fromNum b) |> toNum
            | a, b when a = ZERO || b = ZERO -> ZERO |> simplify
            | a, b when a = ONE -> b |> simplify
            | a, b when b = ONE -> a |> simplify
            | _ -> e
        | Div(x, y) as e -> 
            match x, y with
            | a, b when b = ONE -> a |> simplify
            | a, b when b = ZERO -> failwith "Division by zero error."
            | a, b when isNum a && isNum b -> (fromNum a) / (fromNum b) |> toNum
            | _ -> e
        | Pow(x, y) as e -> 
            match x, y with
            | a, b when b = ZERO -> ONE
            | a, b when b = ONE -> a |> simplify
            | a, b when a = ZERO -> ZERO
            | a, b when a = ONE -> ONE
            | a, b when isNum a && isNum b ->
                if ((fromNum b).Denominator) = 1I && 0N < fromNum b && fromNum b < MAX_INT_BIGNUM then
                    bignum.PowN(fromNum a, (fromNum b) |> bignum.ToInt32) |> toNum
                elif  ((fromNum b).Denominator) = 1I && - MAX_INT_BIGNUM < fromNum b && fromNum b < 0N then
                    1N / bignum.PowN(fromNum a, ((fromNum b) + -2N * (fromNum b)) |> bignum.ToInt32) |> toNum
                else Pow(a, b)
            | _ -> e
        | simplifiedExpression -> simplifiedExpression

    /// Evaluates the number stack and operator stack
    let rec private evalStack (currentOperator: operator option) numberStack operatorStack = 
        match numberStack, operatorStack with
        | ns, [] -> ns, []
        | firstNum :: numbers, Prefix(pOp) :: operators -> 
            (pOp.Expression(firstNum) :: numbers, operators) ||> evalStack currentOperator
        | firstNum :: secondNum :: numbers, Infix(iOp) :: operators 
            when currentOperator |> Option.isSome && iOp.Precedence < currentOperator.Value.Precedence -> 
                numberStack, operatorStack
        | firstNum :: secondNum :: numbers, Infix(iOp) :: operators -> 
            (iOp.Expression(secondNum, firstNum) :: numbers, operators) ||> evalStack currentOperator
        | ns, os -> failwithf "Could not evaluate the operator stack.\n%A\n%A" ns os

    /// Converts the morpheme list into an expression
    let rec private morphemesToExpression numberStack operatorStack morphemeList = 
        match morphemeList, numberStack, operatorStack with
        | RightParenthesis :: tail, ns, os -> 
            let ns', os' = (ns, os) ||> evalStack None
            match ns', os' with
            | [e], [] -> e, tail
            | _, _ -> failwith "Could not parse expression."
        | LeftParenthesis :: tail, ns, os ->
            let innerExpression, rest = tail |> morphemesToExpression [] []
            rest |> morphemesToExpression (innerExpression :: ns) os
        | Constant(c) :: rest, ns, os -> 
            rest |> morphemesToExpression (Num(c) :: ns) os
        | Variable(v) :: rest, ns, os -> 
            rest |> morphemesToExpression (Var(v) :: ns) os
        | Prefix(pOp) :: rest, ns, [] -> 
            rest |> morphemesToExpression ns (Prefix(pOp) :: operatorStack)
        | Prefix(pOp) :: rest, ns, Prefix(pOp1) :: operators -> 
            rest |> morphemesToExpression ns (Prefix(pOp) :: operatorStack)
        | Prefix(pOp) :: rest, ns, Infix(iOp1) :: operators when pOp.Precedence <= iOp1.Precedence ->
            if iOp1.Precedence = MAX_PREC then
                rest |> morphemesToExpression ns (Prefix(pOp) :: operatorStack)
            else
                let ns', os' = (numberStack, operatorStack) ||> evalStack (Some(pOp))
                rest |> morphemesToExpression ns' (Prefix(pOp) :: os')
        | Prefix(pOp) :: rest, ns, Infix(iOp1) :: operators -> 
            rest |> morphemesToExpression numberStack (Prefix(pOp) :: operatorStack)
        | Infix(iOp) :: rest, ns, [] -> 
            rest |> morphemesToExpression ns (Infix(iOp) :: operatorStack)
        | Infix(iOp) :: rest, ns, Prefix(pOp1) :: operators when iOp.Precedence <= pOp1.Precedence -> 
            let ns', os' = (numberStack, operatorStack) ||> evalStack (Some(iOp))
            rest |> morphemesToExpression ns' (Infix(iOp) :: os')
        | Infix(iOp) :: rest, ns, Infix(iOp1) :: operators when iOp.Precedence <= iOp1.Precedence ->
            if iOp.Precedence = MAX_PREC && iOp1.Precedence = MAX_PREC then
                rest |> morphemesToExpression ns (Infix(iOp) :: operatorStack)
            else
                let ns', os' = (numberStack, operatorStack) ||> evalStack (Some(iOp))
                rest |> morphemesToExpression ns' (Infix(iOp) :: os')
        | Infix(iOp) :: rest, ns, Infix(iOp1) :: operators -> 
            rest |> morphemesToExpression numberStack (Infix(iOp) :: operatorStack)
        | Infix(iOp) :: rest, ns, Prefix(pOp1) :: operators -> 
            rest |> morphemesToExpression numberStack (Infix(iOp) :: operatorStack)
        | _ -> failwith "Error: could not convert to Expression (maybe check parenstheses?)."

    let private makeSureParsedCorrectly (resultingExpression, resultingMorphemeList) = 
        match resultingMorphemeList with
        | [] -> resultingExpression
        | _  -> failwith "Error: could not convert to Expression (maybe check parenstheses?)."

    /// Converts a string into a morpheme list
    let rec private parse acc lastExpr stringExpr = 
            match stringExpr with
            | [] -> List.rev acc
            | head :: tail when isWhiteSpace head -> 
                parse acc lastExpr tail
            | '(' :: tail ->
                tail |> parse (LeftParenthesis :: acc) (Some(LeftParenthesis))
            | ')' :: tail -> 
                tail |> parse (RightParenthesis :: acc) (Some(RightParenthesis))
            | op :: tail when isOperator op ->
                match lastExpr with
                | None | Some(Infix(_)) | Some(Prefix(_)) when isPrefix op -> 
                    let opr = Prefix(operator.Get(op, 1))
                    tail |> parse (opr :: acc) (Some(opr))
                | Some(Variable(_)) | Some(Constant(_)) 
                | Some(LeftParenthesis) | Some(RightParenthesis) ->
                    let opr = Infix(operator.Get(op, 2))
                    tail |> parse (opr :: acc) (Some(opr))
                | _ -> failwithf "'%c' is not properly placed" op
            | number :: tail when '0' <= number && number <= '9' -> 
                let num, rest = getFirstNumber (number :: tail)
                let rcnst = Constant(RegularConstant(Rational(num)))
                rest |> parse (rcnst :: acc) (Some(rcnst))
            | x :: tail when x = E -> 
                let scnst = Constant(SpecialConstant(sconstant.Euler))
                tail |> parse (scnst :: acc) (Some(scnst))
            | x :: tail when x = PI -> 
                let scnst = Constant(SpecialConstant(sconstant.Pi))
                tail |> parse (scnst :: acc) (Some(scnst)) 
            | var :: tail when allowedVariables |> Set.contains var ->
                let vr = Variable(var) 
                tail |> parse (vr :: acc) (Some(vr))
            | unknown :: _ -> failwithf "'%c' is an unrecognized character" unknown

    let fromString stringExpression =
        stringExpression
        |> sprintf "%s)"
        |> Seq.toList
        |> makeSureExpressionIsMonoVariate
        |> parse [] None
        |> morphemesToExpression [] []
        |> makeSureParsedCorrectly
        |> simplify