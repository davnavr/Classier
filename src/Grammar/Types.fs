namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

type ClassInheritance =
    | MustInherit
    | CanInherit
    | Sealed

type TypeOrMember<'Type, 'Member> =
    | Type of 'Type
    | Member of 'Member

type Class =
    { ClassName: GenericName
      Body: Statement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier list
      Members: ImmutableSortedSet<TypeOrMember<Class, InstanceMember>>
      PrimaryCtor: (Access * Ctor) option
      SelfIdentifier: IdentifierStr
      SuperClass: FullIdentifier option }

type Interface =
    { InterfaceName: GenericName
      Members: ImmutableSortedSet<TypeOrMember<Interface, AbstractMember>>
      SuperInterfaces: FullIdentifier list }

type Module<'Type> =
    { Body: Statement list
      ModuleName: SimpleName
      Members: ImmutableSortedSet<TypeOrMember<'Type, StaticMember>>}

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module
and Module = Module<TypeDef>

type EntryPoint =
    { Body: Statement list
      Origin: Position
      Parameters: ExpParam list }
