[<AutoOpen>]
module rec Classier.NET.Compiler.IR.Types

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.Generic

module Ast = Classier.NET.Compiler.Grammar.Ast

type NamedType =
    | DefinedType of AccessControl.Access * GenType
    | ExternType of Extern.EType

type ResolvedType = TypeSystem.Type<NamedType>

type GenParam =
    { Name: IdentifierStr option
      Type: ResolvedType }
type GenParamTuple = ImmutableList<GenParam>
type GenParamList = ImmutableList<GenParamTuple>

type GenName = Identifier<GenericParam<GenInterface, GenClass>> // TODO: Should also allow the use of EClass and EInterface here!

type GenPrimaryCtor =
    { Body: ImmutableList<unit>
      Parameters: GenParamTuple
       }

type GenCtor<'GenType> =
    { Parameters: GenParamTuple
       }

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

type InterfaceInheritance =
    | DefinedInterface of GenInterface
    | ExternInterface of Extern.EInterface

type InterfaceMembers = MemberSet<GenInterface, GenInterfaceMember>

type InterfaceSet = ImmutableSortedSet<InterfaceInheritance>

type GenClassMember =
    | ClassCtor of GenCtor<GenType>
    | ClassMthd
    | ClassProp

type GenClass =
    { ClassName: GenName
      Interfaces: InterfaceSet
      Members: ClassMembers
      SuperClass: ClassInheritance option
      Syntax: Ast.Class }

type ClassMembers = MemberSet<GenClass, GenClassMember>

type ClassInheritance =
    | DefinedClass of GenClass
    | ExternClass of Extern.EClass

type GenModuleMember =
    | ModuleFunc
    | ModuleOper

type GenModule =
    { Members: ModuleMembers
      ModuleName: IdentifierStr
      Syntax: Ast.Module }

type ModuleMembers = MemberSet<GenType, GenModuleMember>

type MemberSet =
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
    { Parameter: IdentifierStr option
      Body: unit
      Return: EntryPointReturn
      Syntax: Ast.EntryPoint }
