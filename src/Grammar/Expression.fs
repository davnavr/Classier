namespace Classier.NET.Compiler.Grammar

type Expression =
    | AnonFunc of Function<Expression, PStatement<Expression> list, TypeName option>
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list
           Target: Expression |}
    | PrefixOp of string * Expression
    | InfixOp of Expression * string * Expression
    | IdentifierRef of Identifier
    | IfExpr of If<Expression>
    | MatchExpr of Match<Expression>
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NullLit
    | NumLit of NumericLit
    | SelfRef
    | StrLit of string
    | ThrowExpr of Expression
    | TryExpr of Try<Expression>
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of
        {| Target: Expression
           Value: Expression |}

type Statement = PStatement<Expression>
type If = If<Expression>
type Pattern = Pattern<Expression>
type Local = Local<Expression>
type MatchCase = MatchCase<Expression>
type Match = Match<Expression>
type Try = Try<Expression>
type Function<'Body, 'Type> = Function<Expression, 'Body, 'Type>
/// A function whose return type can be inferred.
type InfFunction = Function<Statement list, TypeName option>

module Expression =
    let withPos pos expr = pos, expr
