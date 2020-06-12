module Classier.NET.Compiler.Grammar

open System
open System.Collections.Immutable

type Identifier = Identifier.Identifier<Generic.Generic>
type FullIdentifier = Identifier.FullIdentifier<Generic.Generic>
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

type If<'Expr, 'Stat> =
    { Condition: 'Expr
      Choice1: 'Stat list
      Choice2: 'Stat list }

type Pattern<'Expr> =
    | Constant of 'Expr
    | Default
    | TuplePattern of Param list
    | VarPattern of string * TypeName

type Local<'Expr> =
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

type Statement<'Expr> =
    | Empty
    | IfStatement of If<'Expr, Statement<'Expr>>
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of 'Expr
    | LocalVar of Local<'Expr>
    | MatchStatement of Match<'Expr, Statement<'Expr>>
    | Return of 'Expr
    | Throw of 'Expr option
    | TryStatement of Try<'Expr, Statement<'Expr>>
    | While of 'Expr * Statement<'Expr> list

type Function<'Expr> =
    { Body: Statement<'Expr> list
      Parameters: Param list list
      ReturnType: TypeName }

    static member empty: Function<'Expr> =
        { Body = List.empty
          Parameters = List.empty
          ReturnType = TypeName.Inferred }

type If<'Expr> = If<'Expr, Statement<'Expr>>
type MatchCase<'Expr> = MatchCase<'Expr, Statement<'Expr>>
type Match<'Expr> = Match<'Expr, Statement<'Expr>>
type Try<'Expr> = Try<'Expr, Statement<'Expr>>

type Expression =
    | AnonFunc of Function<Expression>
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | FuncComp of Expression * Expression
    | IdentifierRef of Identifier
    | IfExpr of If<Expression>
    | MatchExpr of Match<Expression>
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NumLit of NumericLit
    | StrLit of string
    | ThrowExpr of Expression
    | TryExpr of Try<Expression>
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of
        {| Target: Expression
           Value: Expression |}

type Statement = Statement<Expression>
type If = If<Expression>
type Pattern = Pattern<Expression>
type Local = Local<Expression>
type MatchCase = MatchCase<Expression>
type Match = Match<Expression>
type Try = Try<Expression>
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

type TypeDef<'Member> =
    | Class of
        {| ClassName: Name
           Body: Statement list
           Interfaces: FullIdentifier list
           Members: ImmutableList<'Member>
           PrimaryCtor: Constructor
           SuperClass: FullIdentifier |}
    | Interface of
        {| InterfaceName: Name
           Members: ImmutableList<'Member>
           SuperInterfaces: FullIdentifier list |}
    | Module of
        {| Body: Statement list
           ModuleName: Name
           Members: ImmutableList<'Member> |}

type MemberDef =
    | Ctor of Constructor
    | Function of FunctionDef
    | Method of FunctionDef
    | Type of TypeDef<MemberDef>

    static member name mdef =
        match mdef with
        | Function fdef
        | Method fdef -> Some fdef.FuncDef
        | Type tdef ->
            match tdef with
            | Class cdef -> cdef.ClassName
            | Interface idef -> idef.InterfaceName
            | Module mdef -> mdef.ModuleName
            |> Some
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
      Usings: FullIdentifier list }
