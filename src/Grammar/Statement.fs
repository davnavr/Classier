namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type Pattern<'Expr> =
    | Constant of 'Expr
    | Default
    | TuplePattern of InfParam list
    | VarPattern of IdentifierStr * TypeName option

type Local<'Expr> = Pattern<'Expr> * 'Expr

type PStatement<'Expr> = FParsec.Position * Statement<'Expr>
and Statement<'Expr> =
    | Empty
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of 'Expr
    | LetDecl of Local<'Expr>
    | Return of 'Expr
    | VarDecl of Local<'Expr>
    | While of 'Expr * PStatement<'Expr> list
