﻿module Classier.NET.Compiler.Grammar

open System
open System.Collections.Generic
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
      Position: FParsec.Position }

    static member OfString str pos =
        match Identifier.ofString str with
        | Some id ->
            { Identifier = id
              Position = pos }
            |> Some
        | None -> None

    override this.ToString() = this.Identifier.ToString()

type Param =
    { Name: string // TODO: Use string option in order to allow ignored parameters with a _
      Type: TypeName }

    static member OfName name ptype =
        { Name = name
          Type = ptype }

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
        {| Arguments: Expression list
           Target: Expression |}
    | PrefixOp of string * Expression
    | InfixOp of Expression * string * Expression
    | IdentifierRef of Identifier
    | IfExpr of If<Expression>
    | InvalidExpr of string
    | MatchExpr of Match<Expression>
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NullLit
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

type EntryPoint =
    { Body: Statement list
      Origin: FParsec.Position
      Parameters: Param list }

type ConstructorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list

type Constructor =
    { BaseCall: ConstructorBase
      Body: Statement list
      Parameters: Param list }

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
           SelfIdentifier: string
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
type MethodInheritance =
    | Abstract
    | Sealed

type MethodImpl =
    | AbstractOrSealed of MethodInheritance
    | Override of MethodInheritance option
    | Virtual
    // | Inline

type MethodModifiers =
    { ImplKind: MethodImpl
      IsMutator: bool }

    static member Default =
        { ImplKind = AbstractOrSealed MethodInheritance.Sealed
          IsMutator = false }

type PropertyAccessors =
    | AutoGetSet
    | AutoGet
    | GetSet of Statement list * Param * Statement list
    | Get of Statement list

type MemberDef =
    | Ctor of Constructor
    | Function of
        {| Function: Function
           FunctionName: Name |}
    | Method of
        {| Method: Function
           MethodName: Name
           Modifiers: MethodModifiers
           SelfIdentifier: string option |}
    | Property of
        {| Accessors: PropertyAccessors
           PropName: Name
           SelfIdentifier: string option
           Value: Expression option
           ValueType: TypeName |}
    | Type of TypeDef<Access * MemberDef>

module MemberDef =
    let name mdef =
        match mdef with
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
                    compare
                        (firstParams m1)
                        (firstParams m2)
                match paramCompare with
                | 0 ->
                    match (m1, m2) with
                    | (Function _, Method _) -> -1
                    | (Method _, Function _) -> 1
                    | (_, _) ->
                        compare
                            (identifier m1)
                            (identifier m2)
                | _ -> paramCompare }

    let emptyMemberSet = ImmutableSortedSet.Empty.WithComparer memberComparer

    let placeholderCtor cparams =
        { BaseCall = ConstructorBase.SuperCall List.empty
          Body = List.empty
          Parameters = cparams }

    let placeholderFunc name fparams =
        Function
            {| Function =
                { Body = List.empty
                  Parameters = fparams
                  ReturnType = TypeName.Inferred }
               FunctionName = name |}

    let placeholderProp name =
        Property
            {| Accessors = AutoGet
               PropName = name
               SelfIdentifier = None
               Value = None
               ValueType = TypeName.Inferred |}

    let placeholderMethod name selfid mparams =
        Method
            {| Method =
                { Body = List.empty
                  Parameters = mparams
                  ReturnType = TypeName.Inferred }
               MethodName = name
               Modifiers = MethodModifiers.Default
               SelfIdentifier = selfid |}

type Member = Access * MemberDef
type TypeDef = TypeDef<Member>

module TypeDef =
    let placeholderClass name =
        Class
            {| ClassName = name
               Body = List.empty
               Inheritance = ClassInheritance.Sealed
               Interfaces = List.empty
               Members = ImmutableSortedSet.Empty // NOTE: These empty sorted sets don't have the correct comparer for members.
               PrimaryCtor = Access.Public, (MemberDef.placeholderCtor List.empty)
               SelfIdentifier = "this"
               SuperClass = None |}

    let placeholderInterface name =
        Interface
            {| InterfaceName = name
               Members = ImmutableSortedSet.Empty
               SuperInterfaces = List.empty |}

    let placeholderModule name =
        Module
            {| Body = List.empty
               ModuleName = name
               Members = ImmutableSortedSet.Empty |}

type CompilationUnit =
    { EntryPoint: EntryPoint option
      Namespace: FullIdentifier
      Usings: FullIdentifier list
      Types: seq<Access * TypeDef> }
