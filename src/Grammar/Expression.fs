namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

type If<'Expr> =
    { Condition: 'Expr
      Choice1: PStatement<'Expr> list
      Choice2: PStatement<'Expr> list }

type MatchCase<'Expr> =
    { Body: PStatement<'Expr> list
      Patterns: Pattern<'Expr> list }

type Match<'Expr> =
    { Against: 'Expr
      Cases: MatchCase<'Expr> list }

type Try<'Expr> =
    { TryBody: PStatement<'Expr> list
      Handlers: MatchCase<'Expr> list
      Finally: PStatement<'Expr> list }

type Signature<'Body, 'Type> =
    { Body: 'Body
      Parameters: Param<'Type> list list
      ReturnType: 'Type }

type Expression =
    | AnonFunc of Signature<PStatement<Expression> list, TypeName option>
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list
           Target: Expression |}
    | PrefixOp of string * Expression
    | InfixOp of Expression * string * Expression
    | IdentifierRef of Identifier<Generic.Generic>
    | IfExpr of If<Expression>
    | MatchExpr of Match<Expression>
    | MemberAccess of Expression * Identifier<Generic.Generic>
    | Nested of Expression
    | NullLit
    | NumLit of NumericLit
    | StrLit of string
    | ThrowExpr of Expression option
    | TryExpr of Try<Expression>
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of
        {| Target: Expression
           Value: Expression |}

type PStatement = PStatement<Expression>
type If = If<Expression>
type Pattern = Pattern<Expression>
type Local = Local<Expression>
type MatchCase = MatchCase<Expression>
type Match = Match<Expression>
type Try = Try<Expression>
/// A function whose return type can be inferred.
type InfFunction = Signature<PStatement list, TypeName option>

module Expression =
    let withPos pos expr = pos, expr
