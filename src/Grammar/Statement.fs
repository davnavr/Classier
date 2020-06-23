namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type Pattern<'Expr> =
    | Constant of 'Expr
    | Default
    | TuplePattern of InfParam list
    | VarPattern of IdentifierStr * TypeName option

[<RequireQualifiedAccess>]
type Local<'Expr> =
    | Let of Pattern<'Expr> * 'Expr
    | Var of Pattern<'Expr> * 'Expr option // TODO: Require 'var' to also have an expression, just like how let mutable works in F#.

type PStatement<'Expr> = FParsec.Position * Statement<'Expr>
and Statement<'Expr> = // TODO: Make if, while, match, throw, and try only be expressions.
    | Empty
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of 'Expr
    | LocalVar of Local<'Expr>
    | Return of 'Expr
    | While of 'Expr * PStatement<'Expr> list
