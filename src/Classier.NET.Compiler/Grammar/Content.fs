namespace Classier.NET.Compiler.Grammar

type Definition =
    { Flags: Flags
      Identifier: Identifier
      Position: FParsec.Position }

    override this.ToString() = this.Identifier.ToString()

type Expression =
    | AnonFunc of Function
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | FuncComp of Expression * Expression
    | IdentifierRef of Identifier
    | IfExpr of If
    | MatchExpr of Match
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NumLit of NumLiteral
    | StrLit of string
    | ThrowExpr of Expression
    | TryExpr of Try
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of Assignment
and Assignment =
    { Target: Expression
      Value: Expression }
and If =
    { Condition: Expression
      Choice1: Statement list
      Choice2: Statement list }
and Match =
    { Against: Expression
      Cases: MatchCase list }
and MatchCase =
    { Body: Statement list
      Patterns: Pattern list }
and Pattern =
    | Constant of Expression
    | Default
    | TuplePattern of Param list
    | VarPattern of string * TypeName
and Statement =
    | Empty
    | IfStatement of If
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    /// Used in the body of classes and modules.
    | LocalMember of MemberDef
    | LocalVar of Variable
    | MatchStatement of Match
    | Return of Expression
    | Throw of Expression option
    | TryStatement of Try
    | While of Expression * Statement list
and Try =
    { TryBody: Statement list
      Handlers: MatchCase list
      Finally: Statement list }
and Variable =
    { Pattern: Pattern
      VarFlags: Flags
      Value: Expression option }
and Param =
    { Name: string
      Type: TypeName }
and Function =
    { Body: Statement list
      FuncDef: Definition option
      Parameters: Param list list
      ReturnType: TypeName }
and ClassHeader =
    { PrimaryCtor: Constructor
      SuperClass: Identifier list }
and TypeHeader =
    | Class of ClassHeader
    | Interface
    | Module
and TypeDef =
    { Definition: Definition
      Header: TypeHeader
      InitBody: Statement list
      Interfaces: TypeName list
      Members: MemberDef list }
and Constructor =
    { BaseCall: ConstructorBase
      Body: Statement list
      CtorFlags: Flags
      Parameters: Param list }
and ConstructorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list
and MemberDef =
    | Ctor of Constructor
    | Function of Function
    | Property of 
        {| Get: Function option
           PropDef: Definition
           Set: Function option
           Value: Expression option |}
    | Method of Function
    | NestedType of TypeDef
