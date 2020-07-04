namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Identifier

type TypeOrMember<'Type, 'Member> = Grammar.TypeOrMember<'Type, 'Member>

type GenInterfaceMember<'GenType> =
    | InterfaceMthd
    | InterfaceProp
type GenInterface<'GenType> =
    { InterfaceName: Identifier<unit> // TODO: Create a new type for generic parameters that are validated.
      Members: InterfaceMembers<'GenType>
      SuperInterfaces: InterfaceSet<'GenType>
      Syntax: Grammar.Interface }
and InterfaceInheritance<'GenType> =
    | DefinedInterface of GenInterface<'GenType>
    | ExternInterface of Extern.EInterface
and InterfaceMembers<'GenType> =
    ImmutableSortedSet<TypeOrMember<GenInterface<'GenType>, GenInterfaceMember<'GenType>>>
and InterfaceSet<'GenType> = ImmutableSortedSet<InterfaceInheritance<'GenType>>

type GenClassMember<'GenType> =
    | ClassCtor of GenCtor<'GenType>
    | ClassMthd
    | ClassProp
type GenClass<'GenType> =
    { ClassName: Identifier<unit>
      Interfaces: InterfaceSet<'GenType>
      Members: ClassMembers<'GenType>
      SuperClass: ClassInheritance<'GenType> option
      Syntax: Grammar.Class }
and ClassMembers<'GenType> =
    ImmutableSortedSet<Access * TypeOrMember<GenClass<'GenType>, GenClassMember<'GenType>>>
and ClassInheritance<'GenType> =
    | DefinedClass of GenClass<'GenType>
    | ExternClass of Extern.EClass

type GenModuleMember =
    | ModuleFunc
    | ModuleOper
type GenModule<'GenType> =
    { Members: ModuleMembers<'GenType>
      ModuleName: IdentifierStr
      Syntax: Grammar.Module }
and ModuleMembers<'GenType> =
    ImmutableSortedSet<Access * TypeOrMember<'GenType, GenModuleMember>>

type GenType =
    | GenClass of GenClass<GenType>
    | GenInterface of GenInterface<GenType>
    | GenModule of GenModule<GenType>

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
