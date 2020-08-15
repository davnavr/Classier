[<AutoOpen>]
module rec Classier.NET.Compiler.Grammar.Ast

open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.TypeSystem

type TypeParam =
    | TypeParam of GenericParam<FullIdentifier<TypeName>, FullIdentifier<TypeName>>

    override this.ToString() =
        let (TypeParam id) = this
        id.ToString()

type TypeName =
    | TypeName of Type<FullIdentifier<TypeName>>

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

type PStatement = FParsec.Position * Statement
type Statement =
    | Empty
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    | LetDecl of Pattern * Expression
    | Return of Expression
    | VarDecl of Pattern * Expression option
    | While of Expression * PStatement list

type EntryPoint =
    { Parameters: ExpParam list
      Body: PStatement list
      Origin: FParsec.Position }

type Signature<'Body, 'ReturnType> =
    { Body: 'Body
      Parameters: ExpParam list list
      ReturnType: 'ReturnType }
type Signature<'ReturnType> =
    Signature<PStatement list, 'ReturnType>

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
    | AnonFunc of Signature<TypeName option>
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list
           Target: Expression |}
    | PrefixOp of string * Expression
    | InfixOp of Expression * string * Expression
    | IdentifierRef of Identifier<TypeName>
    | IfExpr of If
    | MatchExpr of Match
    | MemberAccess of Expression * Identifier<TypeName>
    | Nested of Expression
    | NullLit
    | NumLit of NumericLit
    | StrLit of string
    | ThrowExpr of Expression option
    | TryExpr of Try
    | TupleLit of Expression * Expression list
    | UnitLit
    | VarAssignment of
        {| Target: Expression
           Value: Expression |}

type OperatorKind =
    | Infix
    | Prefix

type OperatorStr = // TODO: Move this to Grammar.Operator module and make it have a private ctor.
    | OperatorStr of string

    override this.ToString() =
        let (OperatorStr str) = this
        str

type Operator =
    { Kind: OperatorKind
      Symbol: OperatorStr
      Signature: Signature<TypeName option> }

type MutatorModf =
    | IsMutator
    | IsPure

type Ctor =
    { Call: Expression
      Parameters: ExpParam list
      SelfIdentifier: IdentifierStr option }

type PrimaryCtor = Access * ExpParam list * Expression list

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
    { Method: Signature<TypeName option>
      MethodName: GenericName
      Modifiers: MethodModifiers
      SelfIdentifier: IdentifierStr option }

type PropAccessors = // TODO: Allow different access modifiers for accessors.
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
    { Function: Signature<TypeName option>
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
    | NestedClass of Class
    | NestedInterface of Interface
    | NestedModule of Module

type MemberName =
    | IdentifierName of GenericName
    | OperatorName of OperatorStr

type ClassKind =
    /// A class that has properties already defined.
    | DataClass
    | NormalClass

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
    { ClassKind: ClassKind
      ClassName: GenericName
      Body: PStatement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier<TypeName> list
      Members: MemberList<Class, InstanceMember>
      PrimaryCtor: PrimaryCtor
      SelfIdentifier: IdentifierStr option
      SuperClass: FullIdentifier<TypeName> option }

    override this.ToString() =
        sprintf "%Oclass %O" this.Inheritance this.ClassName

type Interface =
    { InterfaceName: GenericName
      Members: MemberList<Interface, AbstractMember>
      SuperInterfaces: FullIdentifier<TypeName> list }

type Module =
    { ModuleName: SimpleName
      Members: (Access * StaticMember) list }

/// Represents a type or member
type Decl =
    | Class of Class
    | Interface of Interface
    | Module of Module

type CompilationUnit =
    { Declarations: (GlobalAccess * Decl) list
      Namespace: Namespace
      Usings: (FParsec.Position * FullIdentifier<TypeName>) list
      Source: string }
