[<AutoOpen>]
module rec Classier.NET.Compiler.IR.Types

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.Generic

module Ast = Classier.NET.Compiler.Grammar.Ast

type ResolvedType =
    TypeSystem.Type<DefinedOrExtern<GenType, EType>>

type GenParam =
    { Name: IdentifierStr option
      Syntax: Ast.InfParam
      Type: ResolvedType }

type GenParamTuple = ImmutableList<GenParam>
type GenParamList = ImmutableList<GenParamTuple>

type ResolvedInterface =
    Namespace * DefinedOrExtern<GenInterface, EInterface>
type ResolvedClass =
    DefinedOrExtern<GenClass, EClass> // TODO: Add namespace for here too?

type GenName = Identifier<GenericParam<ResolvedInterface, ResolvedClass>>

type GenSignature =
    { Body: GenBody<ResolvedType>
      Parameters: GenParamList
      ReturnType: ResolvedType }

type CallExpression<'Target> =
    { Arguments: ImmutableList<GenExpression>
      Target: 'Target }

type ComplexExpression =
    | CtorCall of CallExpression<ResolvedClass>
type GenExpression =
    | BoolLit of bool
    | ComplexExpr of ComplexExpression

type GenStatement =
    | Empty
    | IgnoredExpr of GenExpression

type GenBody<'ReturnType> =
    { Statements: ImmutableList<GenStatement * Ast.Statement>
      ReturnType: 'ReturnType }

type GenPrimaryCtor =
    { Body: GenBody<unit>
      Parameters: GenParamTuple
      Syntax: Ast.PrimaryCtor }

type PrimaryCtorCall =
    | PrimaryCtorCall of ImmutableList<GenExpression>

type GenCtor =
    { Parameters: GenParamTuple
      SelfCall: PrimaryCtorCall
      Syntax: Ast.Ctor }

type GenMethod =
    | AbstractMthd
    | ConcreteMthd

type GenFunction =
    { FunctionName: GenName
      Signature: GenSignature
      Syntax: Ast.StaticFunction }

type MemberSet<'Type, 'Member> =
    ImmutableSortedSet<Access * TypeOrMember<'Type, 'Member>>

type GenInterfaceMember =
    | InterfaceMthd
    | InterfaceProp

type GenInterface<'Parent> =
    { InterfaceName: GenName
      Members: InterfaceMembers
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

type InterfaceMembers = MemberSet<GenNestedInterface, GenInterfaceMember>
type InterfaceSet = ImmutableSortedSet<ResolvedInterface>

type GenClassMember =
    | ClassCtor of GenCtor
    | ClassMthd of GenMethod
    | ClassProp

type GenClass<'Parent> =
    { ClassName: GenName
      Interfaces: InterfaceSet
      Members: ClassMembers
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

type ClassMembers = MemberSet<GenNestedClass, GenClassMember>

type GenModuleMember =
    | ModuleFunc of GenFunction
    | ModuleOper

type GenModule<'Parent> =
    { Members: ModuleMembers
      ModuleName: IdentifierStr
      Parent: 'Parent
      Syntax: Ast.Module }

[<RequireQualifiedAccess>]
type GenModule =
    | Nested of GenNestedModule
    | Global of GenGlobalModule

type GenNestedModule = GenModule<GenModule>
type GenGlobalModule = GenModule<Namespace>

type ModuleMembers = MemberSet<GenNestedType, GenModuleMember>

type GenType =
    | GenClass of GenClass
    | GenInterface of GenInterface
    | GenModule of GenModule

type GenNestedType =
    | GenNestedClass of GenNestedClass
    | GenNestedInterface of GenNestedInterface
    | GenNestedModule of GenNestedModule

type GenGlobalType =
    | GenGlobalClass of GenGlobalClass
    | GenGlobalInterface of GenGlobalInterface
    | GenGlobalModule of GenGlobalModule

type EntryPointReturn =
    | ExitCode
    | ImplicitZero

type GenEntryPoint =
    { Parameters: GenParamTuple
      Body: GenBody<EntryPointReturn>
      Syntax: Ast.EntryPoint }

type GenOutput =
    { GlobalTypes: seq<GenGlobalType>
      EntryPoint: GenEntryPoint option }
