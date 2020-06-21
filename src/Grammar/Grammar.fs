namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type private Position = FParsec.Position

[<StructuralEquality>]
[<StructuralComparison>]
type Access =
    | Public
    | Internal
    | Protected
    | Private

type If<'Expr, 'Stat> =
    { Condition: 'Expr
      Choice1: 'Stat list
      Choice2: 'Stat list }

type Pattern<'Expr> =
    | Constant of 'Expr
    | Default
    | TuplePattern of InfParam list
    | VarPattern of IdentifierStr * TypeName option

[<RequireQualifiedAccess>]
type Local<'Expr> =
    | Let of Pattern<'Expr> * 'Expr
    | Var of Pattern<'Expr> * 'Expr option

type MatchCase<'Expr, 'Stat> =
    { Body: 'Stat list
      Patterns: Pattern<'Expr> list }

type Match<'Expr, 'Stat> =
    { Against: 'Expr
      Cases: MatchCase<'Expr, 'Stat> list }

type Try<'Expr, 'Stat> =
    { TryBody: 'Stat list
      Handlers: MatchCase<'Expr, 'Stat> list
      Finally: 'Stat list }

type Statement<'Expr> = // TODO: Make if, while, match, throw, and try only be expressions.
    | Empty
    | IfStatement of If<'Expr, Position * Statement<'Expr>>
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of 'Expr
    | LocalVar of Local<'Expr>
    | MatchStatement of Match<'Expr, Position * Statement<'Expr>>
    | Return of 'Expr
    | Throw of 'Expr option
    | TryStatement of Try<'Expr, Position * Statement<'Expr>>
    | While of 'Expr * (Position * Statement<'Expr>) list

type PStatement<'Expr> = Position * Statement<'Expr>

type Function<'Expr, 'Body, 'Type> =
    { Body: 'Body
      Parameters: Param<'Type> list list
      ReturnType: 'Type }

type If<'Expr> = If<'Expr, PStatement<'Expr>>
type MatchCase<'Expr> = MatchCase<'Expr, PStatement<'Expr>>
type Match<'Expr> = Match<'Expr, PStatement<'Expr>>
type Try<'Expr> = Try<'Expr, PStatement<'Expr>>
