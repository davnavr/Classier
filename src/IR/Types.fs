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
    { Body: GenBody
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

type GenBody =
    { Statements: ImmutableList<GenStatement * Ast.Statement>
      ReturnType: ResolvedType }

type GenPrimaryCtor =
    { Body: GenBody
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

type GenInterface =
    { InterfaceName: GenName
      Members: InterfaceMembers
      SuperInterfaces: InterfaceSet
      Syntax: Ast.Interface }

type InterfaceMembers =
    MemberSet<GenInterface, GenInterfaceMember>
type InterfaceSet = ImmutableSortedSet<ResolvedInterface>

type GenClassMember =
    | ClassCtor of GenCtor
    | ClassMthd of GenMethod
    | ClassProp

type GenClass =
    { ClassName: GenName
      Interfaces: InterfaceSet
      Members: ClassMembers
      PrimaryCtor: GenPrimaryCtor
      SuperClass: DefinedOrExtern<GenClass, EClass> option
      Syntax: Ast.Class }

type ClassMembers = MemberSet<GenClass, GenClassMember>

type GenModuleMember =
    | ModuleFunc of GenFunction
    | ModuleOper

type GenModule =
    { Members: ModuleMembers
      ModuleName: IdentifierStr
      Syntax: Ast.Module }

type ModuleMembers = MemberSet<GenType, GenModuleMember>

type MemberSet = // TODO: Is this type necessary?
    | ClassMembers of ClassMembers
    | InterfaceMembers of InterfaceMembers
    | ModuleMembers of ModuleMembers

type GenType =
    | GenClass of GenClass
    | GenInterface of GenInterface
    | GenModule of GenModule

type EntryPointReturn =
    | ExitCode
    | ImplicitZero

type GenEntryPoint =
    { Parameters: GenParamTuple
      Body: GenBody
      Return: EntryPointReturn
      Syntax: Ast.EntryPoint }

type GenOutput =
    { GlobalTypes: seq<Namespace * GenType>
      EntryPoint: GenEntryPoint option }
