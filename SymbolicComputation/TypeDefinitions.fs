module TypeDefinitions


/// Unicade character for pi
let PI = '\u03C0'

/// Unicode character for e
let E = '\u0065'

/// Numerical constants
[<StructuredFormatDisplay("{AsString}")>]
type sconstant = 
| Euler
| Pi
with
    override self.ToString() = 
        match self with
        | Euler -> sprintf "%c" E
        | Pi -> sprintf "%c" PI
    member m.AsString = m.ToString() 

/// Rational numbers
type rconstant = 
| Rational of bignum

/// Numbers
type constant = 
| SpecialConstant of sconstant
| RegularConstant of rconstant 

/// Mathematical expression
[<StructuredFormatDisplay("{AsString}")>]
type expression = 
| Add of expression * expression
| Sub of expression * expression
| Mul of expression * expression
| Div of expression * expression
| Pow of expression * expression
| Neg of expression
| Pos of expression
| Var of char
| Num of constant
with
    override self.ToString() =
        match self with
        | Var(v) -> sprintf "%c" v
        | Num(RegularConstant(Rational(n))) -> sprintf "%s" (string n)
        | Num(SpecialConstant(n)) -> sprintf "%s" (string n)
        | Neg(e) -> sprintf "-(%s)" (string e)
        | Pos(e) -> sprintf "(%s)" (string e)
        | Add(e1, e2) -> sprintf "(%s) + (%s)" (string e1) (string e2)
        | Sub(e1, e2) -> sprintf "(%s) - (%s)" (string e1) (string e2)
        | Mul(e1, e2) -> sprintf "(%s) * (%s)" (string e1) (string e2)
        | Div(e1, e2) -> sprintf "(%s) / (%s)" (string e1) (string e2)
        | Pow(e1, e2) -> sprintf "(%s)^(%s)" (string e1) (string e2)
    member m.AsString = m.ToString() 

/// Equality of two expressions
type equation = 
| Eq of expression * expression

/// Mathematical operators
[<RequireQualifiedAccess>]
type operator = 
| Neg  
| Pos
| Add
| Sub
| Mul
| Div
| Pow
with 
    /// Operator to expression (binary operators)
    member self.Expression(a, b) = 
        match self with
        | Add -> expression.Add(a, b)
        | Sub -> expression.Sub(a, b)
        | Mul -> expression.Mul(a, b)
        | Div -> expression.Div(a, b)
        | Pow -> expression.Pow(a, b)
        | _ -> failwith "wrong number of arguments to expression"
    /// Operator to expression (unary operators)
    member self.Expression a = 
        match self with
        | Neg -> expression.Neg a
        | Pos -> expression.Pos a
        | _ -> failwith "wrong number of arguments to expression"
    /// String representation of the operator
    member self.Symbol =
        match self with
        | Neg -> '-'
        | Pos -> '+'
        | Add -> '+'
        | Sub -> '-'
        | Mul -> '*'
        | Div -> '/'
        | Pow -> '^'
    /// Precedence of the operator
    member self.Precedence =
        match self with
        | Neg -> 3
        | Pos -> 3
        | Add -> 1
        | Sub -> 1
        | Mul -> 2
        | Div -> 2
        | Pow -> 4
    /// Static member that gets the operator based on the character and the amount of arguments it takes
    static member Get(character, args) = 
        match character, args with
        | '-', 1 -> Neg
        | '+', 1 -> Pos
        | '+', 2 -> Add
        | '-', 2 -> Sub
        | '*', 2 -> Mul
        | '/', 2 -> Div
        | '^', 2 -> Pow
        | op, y -> failwithf "'%c', '%d' is not a valid operator" op y
    /// Static member that has the same functionality of self.Symbol
    static member Get(operator) =
        match operator with
        | Neg -> '-'
        | Pos -> '+'
        | Add -> '+'
        | Sub -> '-'
        | Mul -> '*'
        | Div -> '/'
        | Pow -> '^'

(* 
    TODO: add more overloaded operators
    THIS IS NOT COMPLETE BY ANY MEANS!!!
*)
//type expression with
//    static member (~-) a     = Neg(a)
//    static member (~-) a     = if a = 0N then toNum 0N else Neg(toNum a)
//    static member (~+) a     = Pos(a)
//    static member (~+) a     = if a = 0N then toNum 0N else Neg(toNum a)
//     
//    static member (+) (a, b) = toNum (a + b)
//    static member (+) (a, b) = if a = 0N then b else Add(toNum a, b)
//    static member (+) (a, b) = if b = 0N then a else Add(a, toNum b)
//    static member (+) (a, b) = Add(a, b)
//
//    static member (-) (a, b) = toNum (a + b)
//    static member (-) (a, b) = if a = 0N then -b else Sub(toNum a, b)
//    static member (-) (a, b) = if b = 0N then -a else Sub(a, toNum b)
//    static member (-) (a, b) = Sub(a, b)

/// A single unit of a mathematical expression
type morpheme = 
| Prefix of operator
| Infix of operator
| Variable of char
| Constant of constant
| LeftParenthesis
| RightParenthesis