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

type TypeOrMemberSet<'Type> = ImmutableSortedSet<TypeOrMember<'Type, InstanceMember>>

type Class =
    { ClassName: Name
      Body: Statement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier list
      Members: TypeOrMemberSet<Class>
      PrimaryCtor: (Access * Ctor) option
      SelfIdentifier: IdentifierStr
      SuperClass: FullIdentifier option }

type Interface =
    { InterfaceName: Name
      Members: TypeOrMemberSet<Interface>
      SuperInterfaces: FullIdentifier list }

type Module<'Type> =
    { Body: Statement list
      ModuleName: Name
      Members: ImmutableSortedSet<TypeOrMember<'Type, StaticMember>>}

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module<TypeDef>
