namespace rec Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.Generic

type GenName = Identifier<GenericParam<GenInterface, GenClass>> // TODO: Should also allow the use of EClass and EInterface here!

type MemberSet<'Type, 'Member> =
    ImmutableSortedSet<Access * Grammar.TypeOrMember<'Type, 'Member>>

type GenInterfaceMember =
    | InterfaceMthd
    | InterfaceProp

type GenInterface =
    { InterfaceName: GenName
      Members: InterfaceMembers
      SuperInterfaces: InterfaceSet
      Syntax: Grammar.Interface }

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
      Syntax: Grammar.Class }

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
      Syntax: Grammar.Module }

type ModuleMembers = MemberSet<GenType, GenModuleMember>

type GenType =
    | GenClass of GenClass
    | GenInterface of GenInterface
    | GenModule of GenModule

module GenType =
    let name gtype =
        match gtype with
        | GenClass gclass -> gclass.ClassName
        | GenInterface gintf -> gintf.InterfaceName
        | GenModule gmodl -> Identifier.ofStr gmodl.ModuleName

    let syntax gtype =
        match gtype with
        | GenClass gclass -> Grammar.Class gclass.Syntax
        | GenInterface gintf -> Grammar.Interface gintf.Syntax
        | GenModule gmodl -> Grammar.Module gmodl.Syntax
