module Classier.NET.Compiler.Grammar

open System
open System.Collections.Immutable

type Identifier = Identifier.Identifier<Generic.Generic>
type TypeName = TypeSystem.TypeName<Generic.Generic>

[<AutoOpen>]
module Numeric =
    [<RequireQualifiedAccess>]
    type NumBase =
        | Binary
        | Decimal
        | Hexadecimal

    [<RequireQualifiedAccess>]
    [<StructuredFormatDisplay("{Item}")>]
    type IntegralVal =
        | Byte of int8
        | UByte of uint8
        | Short of int16
        | UShort of uint16
        | Int of int32
        | UInt of uint32
        | Long of int64
        | ULong of uint64

    [<RequireQualifiedAccess>]
    type FPointVal =
        | Float of float
    
    [<StructuredFormatDisplay("{Value}")>]
    type IntegralLit =
        { Base: NumBase
          Value: IntegralVal }

    type NumericLit =
        | Integral of IntegralLit
        | FPointVal of FPointVal

type Name =
    { Identifier: Identifier
      Position: FParsec.Position }

    static member empty =
        { Identifier = { Name = String.Empty; Generics = [] }
          Position = FParsec.Position(String.Empty, 0L, 0L, 0L) }

    override this.ToString() = this.Identifier.ToString()

type If<'Expr, 'Stat> =
    { Condition: 'Expr
      Choice1: 'Stat list
      Choice2: 'Stat list }

type Param =
    { Name: string
      Type: TypeName }

    static member toString (paramSet: Param list) =
        paramSet
        |> Seq.map string
        |> String.concat ", "
        |> sprintf "(%s)"

    static member toString (paramSets: Param list list) =
        paramSets
        |> Seq.map (Param.toString)
        |> String.concat " "

    override this.ToString () =
        match this.Type with
        | TypeName.Inferred -> String.Empty
        | _ -> sprintf " : %s" (string this.Type)
        |> sprintf "%s%s" this.Name

type Pattern<'Expr> =
    | Constant of 'Expr
    | Default
    | TuplePattern of Param list
    | VarPattern of string * TypeName

and Local<'Expr> =
    | Let of Pattern<'Expr> * 'Expr
    | Var of
        {| Name: string
           Type: TypeName
           Value: 'Expr option |}

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

type Function<'Stat> =
    { Body: 'Stat list
      Parameters: Param list list
      ReturnType: TypeName }

    static member empty: Function<'Stat> =
        { Body = List.empty
          Parameters = List.empty
          ReturnType = TypeName.Inferred }

type Expression =
    | AnonFunc of Function<Statement>
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | FuncComp of Expression * Expression
    | IdentifierRef of Identifier
    | IfExpr of If<Expression, Statement>
    | MatchExpr of Match<Expression, Statement>
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NumLit of NumericLit
    | StrLit of string
    | ThrowExpr of Expression
    | TryExpr of Try<Expression, Statement>
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of
        {| Target: Expression
           Value: Expression |}
and Statement =
    | Empty
    | IfStatement of If<Expression, Statement>
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    | LocalVar of Local<Expression>
    | MatchStatement of Match<Expression, Statement>
    | Return of Expression
    | Throw of Expression option
    | TryStatement of Try<Expression, Statement>
    | While of Expression * Statement list

type If = If<Expression, Statement>
type Function = Function<Expression>

type FunctionDef =
    { Function: Function
      FuncDef: Name
      SelfIdentifier: string option }

    static member generatedDef =
        { Function = Function.empty
          FuncDef = Name.empty
          SelfIdentifier = Some "&this" }

    static member placeholder def fparams =
        { Function = { Function.empty with Parameters = fparams }
          FuncDef = def
          SelfIdentifier = None }

type ConstructorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list

type Constructor =
    { BaseCall: ConstructorBase
      Body: Statement list
      Parameters: Param list }

type ClassHeader =
    { PrimaryCtor: Constructor
      SuperClass: Identifier list }

type TypeHeader =
    | Class of ClassHeader
    | Interface
    | Module

type TypeDef<'Member> =
    { Header: TypeHeader
      InitBody: Statement list
      Interfaces: TypeName list
      Members: ImmutableSortedSet<'Member>
      TypeDef: Name }

type MemberDef =
    | Ctor of Constructor
    | Function of FunctionDef
    | Method of FunctionDef
    | Type of TypeDef<MemberDef>

    static member name mdef =
        match mdef with
        | Function fdef
        | Method fdef -> Some fdef.FuncDef
        | Type tdef -> Some tdef.TypeDef
        | _ -> None

    static member identifier mdef =
        MemberDef.name mdef
        |> Option.map (fun def -> def.Identifier)

    /// Gets the parameters of the method, function, or constructor.
    static member paramSets mdef =
        match mdef with
        | Ctor ctor -> [ ctor.Parameters ]
        | Function fdef
        | Method fdef -> fdef.Function.Parameters
        | _ -> List.empty

    /// Gets the first parameter set of the method or function, or the parameters of the constructor.
    static member firstParams mdef =
        match mdef with
        | Ctor ctor -> Some ctor.Parameters
        | Function fdef
        | Method fdef -> List.tryHead fdef.Function.Parameters
        | _ -> None
        |> Option.defaultValue List.empty

type TypeDef = TypeDef<MemberDef>

type CompilationUnit =
    { Definitions: TypeDef list
      Namespace: string list
      Usings: Identifier list list }
