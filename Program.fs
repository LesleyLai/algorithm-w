// Learn more about F# at http://fsharp.org

open System

type Literal = EInt of int | EString of String | EBool of bool
type Expr = EIdentifier of String
          | ELiteral of Literal
          | EApp of Expr * Expr
          | ELet of String * Expr * Expr

type Type = TVar of String
          | TBool
          | TInt
          | TString
          | TFun of Type * Type

type Scheme = Scheme of Type


type Printer =
    static member Show(x:Literal) =
        match x with
            | EInt i -> sprintf "%A" i
            | EString s -> sprintf "%A" s
            | EBool b -> sprintf "%A" b
        
    static member Show(x:Expr) =
        match x with
            | EIdentifier id -> id
            | ELiteral lit -> Printer.Show lit
            | EApp (f, x) -> sprintf "(%s %s)" (Printer.Show f) (Printer.Show x)
            | ELet (name, a, b) ->
                sprintf "(let %s=%s;\n %s)" name (Printer.Show(a)) (Printer.Show(b))
            
    static member Show(x:Type) = printf "%A" x


[<EntryPoint>]
let main argv =
    Console.WriteLine (Printer.Show(
                           ELet ("x", ELiteral (EInt 1),
                               EApp (EIdentifier "f", ELiteral (EBool true)))))
    0 // return an integer exit code
