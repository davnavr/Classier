module Classier.NET.Compiler.Grammar

open System
open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

type FullIdentifier = Identifier.FullIdentifier<Generic.Generic>
type Identifier = Identifier.Identifier<Generic.Generic>
type private Position = FParsec.Position
type TypeName = TypeSystem.TypeName<Generic.Generic>

[<AutoOpen>]
module Numeric =
    [<RequireQualifiedAccess>]
    type NumBase =
        | Binary
        | Decimal
        | Hexadecimal
    
    type IntegralLit =
        { Base: NumBase
          Digits: string }

        override this.ToString() =
            let prefix =
                match this.Base with
                | NumBase.Binary -> "0b"
                | NumBase.Decimal -> String.Empty
                | NumBase.Hexadecimal -> "0x"
            sprintf "%s%s"
                prefix
                this.Digits

    type FPointLit =
        { IntDigits: string
          FracDigits: string }

        override this.ToString() =
            sprintf "%s.%s"
                this.IntDigits
                this.FracDigits

    [<RequireQualifiedAccess>]
    type NumericLit =
        /// An integral literal of an unknown type.
        | Integral of IntegralLit
        /// An floating-point literal of an unknown type.
        | FPoint of FPointLit
        | Double of FPointLit
        | Float of FPointLit
        | Long of IntegralLit

[<StructuralEquality>]
[<StructuralComparison>]
type Access =
    | Public
    | Internal
    | Protected
    | Private

type Name =
    { Identifier: Identifier
      Position: Position }

    static member OfStr pos str =
        { Identifier = Identifier.ofStr str
          Position = pos }

    override this.ToString() = this.Identifier.ToString()

type Param<'Type> =
    { Name: IdentifierStr option
      Type: 'Type }

    static member Create ptype name =
        { Name = name
          Type = ptype }

    override this.ToString() =
        let name =
            match this.Name with
            | None -> "_"
            | Some pname -> string pname
        this.Type.ToString()
        |> sprintf "%s%s" name

/// A parameter whose type can be inferred.
type InfParam = Param<TypeName option>
type ExpParam = Param<TypeName>

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

module Expression =
    let withPos pos expr = pos, expr

type EntryPoint =
    { Body: Statement list
      Origin: Position
      Parameters: ExpParam list }

type ConstructorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list

type Constructor =
    { BaseCall: ConstructorBase
      Body: Statement list
      Parameters: InfParam list
      SelfIdentifier: IdentifierStr option }

type ClassInheritance =
    | MustInherit
    | CanInherit
    | Sealed

type TypeDef<'Member> =
    | Class of
        {| ClassName: Name
           Body: Statement list
           Inheritance: ClassInheritance
           Interfaces: FullIdentifier list
           Members: ImmutableSortedSet<'Member>
           PrimaryCtor: Access * Constructor
           SelfIdentifier: IdentifierStr
           SuperClass: FullIdentifier option |}
    | Interface of
        {| InterfaceName: Name
           Members: ImmutableSortedSet<'Member>
           SuperInterfaces: FullIdentifier list |}
    | Module of
        {| Body: Statement list
           ModuleName: Name
           Members: ImmutableSortedSet<'Member> |}

[<RequireQualifiedAccess>]
type MethodImpl =
    | Default
    | Override
    | SealedOverride
    | Virtual

type MethodModifiers =
    { ImplKind: MethodImpl
      IsMutator: bool }

    static member Default =
        { ImplKind = MethodImpl.Default
          IsMutator = false }

type PropertyAccessors =
    | AutoGet
    | AutoGetSet
    | Get of Statement list
    | GetSet of Statement list * InfParam * Statement list

/// A function whose return type can be inferred.
type InfFunction = Function<Statement list, TypeName option>

type MemberDef =
    | AbMethod of
        {| IsMutator: bool
           IsOverride: bool
           Method: Function<unit, TypeName>
           MethodName: Name |}
    | AbProperty of
        {| HasSet: bool
           PropName: Name
           ValueType: TypeName |}
    | Ctor of Constructor
    | Function of
        {| Function: InfFunction
           FunctionName: Name |}
    | Method of
        {| Method: InfFunction
           MethodName: Name
           Modifiers: MethodModifiers
           SelfIdentifier: IdentifierStr option |}
    | Property of
        {| Accessors: PropertyAccessors
           PropName: Name
           SelfIdentifier: IdentifierStr option
           Value: Expression option
           ValueType: TypeName option |}
    | Type of TypeDef<Access * MemberDef>

module MemberDef =
    let name mdef =
        match mdef with
        | AbMethod mdef -> Some mdef.MethodName
        | AbProperty pdef -> Some pdef.PropName
        | Ctor _ -> None
        | Function fdef -> Some fdef.FunctionName
        | Method mdef -> Some mdef.MethodName
        | Property pdef -> Some pdef.PropName
        | Type tdef ->
            match tdef with
            | Class cdef -> cdef.ClassName
            | Interface idef -> idef.InterfaceName
            | Module mdef -> mdef.ModuleName
            |> Some

    let identifier mdef =
        Option.map
            (fun def -> def.Identifier)
            (name mdef)

    /// Gets the parameters of the method, function, or constructor.
    let paramSets mdef =
        match mdef with
        | AbMethod mdef ->
            let paramMap mparam =
                InfParam.Create
                    (Some mparam.Type)
                    mparam.Name
            List.map
                (List.map paramMap)
                mdef.Method.Parameters
        | Ctor ctor -> [ ctor.Parameters ]
        | Function fdef -> fdef.Function.Parameters
        | Method mdef -> mdef.Method.Parameters
        | _ -> List.empty

    /// Gets the first parameter set of the method or function, or the parameters of the constructor.
    let firstParams mdef =
        mdef
        |> paramSets
        |> List.tryHead
        |> Option.defaultValue List.empty

    let memberComparer =
        { new System.Collections.Generic.IComparer<Access * MemberDef> with
            member _.Compare((_, m1), (_, m2)) =
                let paramCompare =
                    List.compareWith
                        (fun p1 p2 ->
                            match (p1.Type, p2.Type) with
                            | (None, _)
                            | (_, None) -> 0
                            | (Some t1, Some t2) -> compare t1 t2)
                        (firstParams m1)
                        (firstParams m2)
                match paramCompare with
                | 0 ->
                    match (m1, m2) with
                    | (Function _, AbMethod _)
                    | (Function _, Method _) -> -1
                    | (AbMethod _, Function _)
                    | (Method _, Function _) -> 1
                    | (_, _) ->
                        compare
                            (identifier m1)
                            (identifier m2)
                | _ -> paramCompare }

    let emptyMemberSet = ImmutableSortedSet.Empty.WithComparer memberComparer

    let placeholderCtor cparams =
        { BaseCall = SuperCall List.empty
          Body = List.empty
          Parameters = cparams
          SelfIdentifier = None }

    let internal placeholderMethod name selfid mparams =
        Method
            {| Method =
                { Body = List.empty
                  Parameters = mparams
                  ReturnType = None }
               MethodName = name
               Modifiers = MethodModifiers.Default
               SelfIdentifier = selfid |}

type Member = Access * MemberDef
type TypeDef = TypeDef<Member>

module internal TypeDef =
    let placeholderClass name =
        Class
            {| ClassName = name
               Body = List.empty
               Inheritance = ClassInheritance.Sealed
               Interfaces = List.empty
               Members = MemberDef.emptyMemberSet
               PrimaryCtor = Access.Public, (MemberDef.placeholderCtor List.empty)
               SelfIdentifier = IdentifierStr "this"
               SuperClass = None |}

    let placeholderInterface name =
        Interface
            {| InterfaceName = name
               Members = MemberDef.emptyMemberSet
               SuperInterfaces = List.empty |}

    let placeholderModule name =
        Module
            {| Body = List.empty
               ModuleName = name
               Members = MemberDef.emptyMemberSet |}

type CompilationUnit =
    { EntryPoint: EntryPoint option
      Namespace: FullIdentifier option
      Usings: FullIdentifier list
      Types: seq<Access * TypeDef> }
