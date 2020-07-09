[<AutoOpen>]
module rec Classier.NET.Compiler.Grammar.Ast

open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

type TypeParam =
    | TypeParam of GenericParam<FullIdentifier<TypeArg>, FullIdentifier<TypeArg>>

    override this.ToString() =
        let (TypeParam id) = this
        id.ToString()

// TODO: Rename this to TypeArgOrParam or remove it.
type TypeArg = Generic<TypeName, FullIdentifier<TypeParam>, FullIdentifier<TypeParam>>

type TypeName =
    | TypeName of Type<FullIdentifier<TypeArg>>

type Name<'Identifier> =
    { Identifier: 'Identifier
      Position: FParsec.Position }

    override this.ToString() = this.Identifier.ToString()

type SimpleName = Name<IdentifierStr>
type GenericName = Name<Identifier<TypeParam>>

type Param<'Type> =
    { Name: IdentifierStr option
      Type: 'Type }

/// A parameter whose type can be inferred.
type InfParam = Param<TypeName option>
type ExpParam = Param<TypeName>

type Pattern =
    | Constant of Expression
    | Default
    | TuplePattern of InfParam list
    | VarPattern of IdentifierStr * TypeName option

type Local = Pattern * Expression

type PStatement = FParsec.Position * Statement
type Statement =
    | Empty
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    | LetDecl of Local
    | Return of Expression
    | VarDecl of Local
    | While of Expression * PStatement list

type EntryPoint =
    { Parameters: ExpParam list
      Body: PStatement list
      Origin: FParsec.Position }

type Signature<'Body, 'Type> =
    { Body: 'Body
      Parameters: Param<'Type> list list
      ReturnType: 'Type }
type InfSignature = Signature<PStatement list, TypeName option>

type If =
    { Condition: Expression
      Choice1: PStatement list
      Choice2: PStatement list }

type MatchCase =
    { Body: PStatement list
      Patterns: Pattern list }
type Match =
    { Against: Expression
      Cases: MatchCase list }

type Try =
    { TryBody: PStatement list
      Handlers: MatchCase list
      Finally: PStatement list }

type Expression =
    | AnonFunc of Signature<PStatement list, TypeName option>
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list
           Target: Expression |}
    | PrefixOp of string * Expression
    | InfixOp of Expression * string * Expression
    | IdentifierRef of Identifier<TypeArg>
    | IfExpr of If
    | MatchExpr of Match
    | MemberAccess of Expression * Identifier<TypeArg>
    | Nested of Expression
    | NullLit
    | NumLit of NumericLit
    | StrLit of string
    | ThrowExpr of Expression option
    | TryExpr of Try
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of
        {| Target: Expression
           Value: Expression |}

type OperatorKind =
    | Infix
    | Prefix

type OperatorStr =
    | OperatorStr of string

    override this.ToString() =
        let (OperatorStr str) = this
        str

type Operator =
    { Body: PStatement list
      Kind: OperatorKind
      Operands: InfParam list
      ReturnType: TypeName option
      Symbol: OperatorStr }

type MutatorModf =
    | IsMutator
    | IsPure

type Ctor =
    { Call: Expression
      Parameters: InfParam list
      SelfIdentifier: IdentifierStr option }

[<RequireQualifiedAccess>]
type MethodImpl =
    | Default
    | Override
    | SealedOverride
    | Virtual

type MethodModifiers =
    { ImplKind: MethodImpl
      Purity: MutatorModf }

    static member Default =
        { ImplKind = MethodImpl.Default
          Purity = IsMutator }

type Method =
    { Method: InfSignature
      MethodName: GenericName
      Modifiers: MethodModifiers
      SelfIdentifier: IdentifierStr option }

type PropAccessors =
    | AutoGet
    | AutoGetSet
    | Get of PStatement list
    | GetSet of PStatement list * InfParam * PStatement list

type Property =
    { Accessors: PropAccessors
      PropName: SimpleName
      SelfIdentifier: IdentifierStr option
      Value: Expression option
      ValueType: TypeName option }

type AMethod =
    { Method: Signature<unit, TypeName>
      MethodName: GenericName }

type AbstractPropAccessors =
    | AbstractGet
    | AbstractGetSet

type AProperty =
    { Accessors: AbstractPropAccessors
      PropName: SimpleName
      Purity: MutatorModf
      ValueType: TypeName }

type StaticFunction =
    { Function: InfSignature
      FunctionName: GenericName }

type AbstractMember =
    | AMethod of AMethod
    | AProperty of AProperty

type ConcreteMember =
    | Constructor of Ctor
    | Method of Method
    | Property of Property

type InstanceMember =
    | Abstract of AbstractMember
    | Concrete of ConcreteMember

type StaticMember =
    | Function of StaticFunction
    | Operator of Operator

type Member =
    | Instance of InstanceMember
    | Static of StaticMember

type MemberName =
    | IdentifierName of GenericName
    | OperatorName of OperatorStr

type ClassInheritance =
    | MustInherit
    | CanInherit
    | Sealed

    override this.ToString() =
        match this with
        | MustInherit -> "abstract "
        | CanInherit -> "inheritable "
        | _ -> ""

type MemberList<'Type, 'Member> = (Access * TypeOrMember<'Type, 'Member>) list

type Class =
    { ClassName: GenericName
      Body: PStatement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier<TypeArg> list
      Members: MemberList<Class, InstanceMember>
      PrimaryCtor: Access * InfParam list * Expression list
      SelfIdentifier: IdentifierStr option
      SuperClass: FullIdentifier<TypeArg> option }

    override this.ToString() =
        sprintf "%Oclass %O" this.Inheritance this.ClassName

type Interface =
    { InterfaceName: GenericName
      Members: MemberList<Interface, AbstractMember>
      SuperInterfaces: FullIdentifier<TypeArg> list }

type Module =
    { ModuleName: SimpleName
      Members: MemberList<TypeDef, StaticMember> }

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module

type CompilationUnit =
    { Namespace: Namespace
      Usings: FullIdentifier<TypeArg> list
      Source: string
      Types: (GlobalAccess * TypeDef) list }
