[<AutoOpen>]
module rec Classier.NET.Compiler.IR.Types

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Generic

open Classier.NET.Compiler.Extern

module Ast = Classier.NET.Compiler.Grammar.Ast

type ResolvedType =
    TypeSystem.Type<DefinedOrExtern<GenType, EType>>

type GenParam =
    { Name: IdentifierStr option
      Syntax: Ast.ExpParam
      Type: ResolvedType }

type GenParamTuple = ImmutableList<GenParam>
type GenParamList = ImmutableList<GenParamTuple>

type ResolvedInterface = DefinedOrExtern<GenInterface, EInterface>
type ResolvedClass = DefinedOrExtern<GenClass, EClass>

type GenName = Identifier<GenericParam<ResolvedInterface, ResolvedClass>>

type GenSignature<'Body> =
    { Body: 'Body
      Parameters: GenParamList
      ReturnType: ResolvedType }

type CallExpression<'Target> =
    { Arguments: ImmutableList<GenExpression>
      Target: 'Target }

type ComplexExpression =
    | CtorCall of CallExpression<ResolvedClass>
    | TempEFunctionCall of CallExpression<Namespace * EGlobalModule * EFunction>
type GenExpression =
    | BoolLit of bool
    | ComplexExpr of ComplexExpression
    | GlobalTypeRef of Namespace * DefinedOrExtern<GenGlobalType, EGlobalType> // TODO: Should every type and member (EGlobalClass, GenNestedInterface, GenCtor, etc.), have its own Ref case here?
    | TempEFunctionRef of Namespace * EGlobalModule * EFunction
    | NamespaceRef of Namespace // TODO: Remove NamespaceRef, it should not be an expression and doesn't have a type.
    | StrLit of string

type GenStatement =
    | Empty
    | IgnoredExpr of GenExpression

type GenBody = ImmutableList<GenStatement * Ast.PStatement>

type GenPrimaryCtor =
    { Body: GenBody
      Parameters: GenParamTuple
      ParentClass: GenClass
      Syntax: Ast.PrimaryCtor }

type PrimaryCtorCall =
    | PrimaryCtorCall of ImmutableList<GenExpression>

type GenCtor<'GenClass> =
    { Parameters: GenParamTuple
      ParentClass: 'GenClass
      SelfCall: PrimaryCtorCall
      Syntax: Ast.Ctor }

type GenMethod<'Parent, 'Body, 'Syntax> =
    { MethodName: GenName
      Parent: 'Parent
      Signature: GenSignature<'Body>
      Syntax: 'Syntax }

type GenAbstractMethod<'Parent> = GenMethod<'Parent, unit, Ast.AMethod>
type GenConcreteMethod<'Parent> = GenMethod<'Parent, GenBody, Ast.Method>

type GenMethod<'Parent> =
    | GenAbstractMethod of GenAbstractMethod<'Parent>
    | GenConcreteMethod of GenConcreteMethod<'Parent>

type GenPropertyAccessors = unit

type GenProperty<'Parent, 'Body, 'Syntax> =
    { Accessors: GenPropertyAccessors
      Parent: 'Parent
      PropName: GenName
      Syntax: Ast.Property
      ValueType: ResolvedType }

type GenAbstractProperty<'Parent> = GenProperty<'Parent, unit, Ast.AProperty>
type GenConcreteProperty<'Parent> = GenProperty<'Parent, GenBody, Ast.Property>

type GenProperty<'Parent> =
    | GenAbstractProperty of GenAbstractProperty<'Parent>
    | GenConcreteProperty of GenConcreteProperty<'Parent>

type GenFunction =
    { FunctionName: GenName
      ParentModule: GenModule
      Signature: GenSignature<GenBody>
      Syntax: Ast.StaticFunction }

type GenOperatorKind =
    | InfixOp of GenSignature<GenBody>
    | PrefixOp of
        {| Body: GenBody
           Operand: GenParam
           ReturnType: ResolvedType |}

type GenOperator =
    { OperatorKind: GenOperatorKind
      ParentModule: GenModule
      Syntax: Ast.Operator }

type MemberSet<'Type, 'Member> =
    ImmutableSortedSet<Access * TypeOrMember<'Type, 'Member>>

type GenInterfaceMember<'GenInterface> =
    | InterfaceMthd of GenAbstractMethod<'GenInterface>
    | InterfaceProp of GenAbstractProperty<'GenInterface>

type GenInterface<'Parent> =
    { InterfaceName: GenName
      Members: MemberSet<GenNestedInterface, GenInterfaceMember<GenInterface<'Parent>>>
      Parent: 'Parent
      SuperInterfaces: InterfaceSet
      Syntax: Ast.Interface }

[<RequireQualifiedAccess>]
type GenInterface =
    | Nested of GenNestedInterface
    | Global of GenGlobalInterface

    member this.InterfaceName =
        match this with
        | Nested n -> n.InterfaceName
        | Global g -> g.InterfaceName

type GenNestedInterface = GenInterface<GenInterface>
type GenGlobalInterface = GenInterface<Namespace>

type InterfaceSet = ImmutableSortedSet<ResolvedInterface>

type GenClassMember<'GenClass> =
    | ClassCtor of GenCtor<'GenClass>
    | ClassMthd of GenMethod<'GenClass>
    | ClassProp of GenProperty<'GenClass>

type GenClass<'Parent> =
    { ClassName: GenName
      Interfaces: InterfaceSet
      Members: MemberSet<GenNestedClass, GenClassMember<GenClass<'Parent>>>
      Parent: 'Parent
      PrimaryCtor: GenPrimaryCtor
      SuperClass: DefinedOrExtern<GenClass, EClass> option
      Syntax: Ast.Class }

[<RequireQualifiedAccess>]
type GenClass =
    | Nested of GenNestedClass
    | Global of GenGlobalClass

    member this.ClassName =
        match this with
        | Nested n -> n.ClassName
        | Global g -> g.ClassName

type GenNestedClass = GenClass<GenClass>
type GenGlobalClass = GenClass<Namespace>

type GenModuleMember =
    | ModuleFunc of GenFunction
    | ModuleOper of GenOperator

type GenModule<'Parent> =
    { Members: MemberSet<GenNestedType<GenModule>, GenModuleMember>
      ModuleName: IdentifierStr
      Parent: 'Parent
      Syntax: Ast.Module }

[<RequireQualifiedAccess>]
type GenModule =
    | Nested of GenNestedModule
    | Global of GenGlobalModule

type GenNestedModule = GenModule<GenModule>
type GenGlobalModule = GenModule<Namespace>

type GenType =
    | GenClass of GenClass
    | GenInterface of GenInterface
    | GenModule of GenModule

type GenNestedType<'Parent> =
    | GenNestedClass of GenClass<'Parent>
    | GenNestedInterface of GenInterface<'Parent>
    | GenNestedModule of GenModule<'Parent>

type GenGlobalType =
    | GenGlobalClass of GenGlobalClass
    | GenGlobalInterface of GenGlobalInterface
    | GenGlobalModule of GenGlobalModule

type ExitCode =
    | Explicit
    | ImplicitZero

type GenEntryPoint =
    { ExitCode: ExitCode
      Parameters: GenParamTuple
      Body: GenBody
      Syntax: Ast.EntryPoint }

type GenOutput =
    { GlobalTypes: seq<GenGlobalType>
      EntryPoint: GenEntryPoint option }
