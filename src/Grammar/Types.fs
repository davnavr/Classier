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

type MemberSet<'Member> = ImmutableSortedSet<Access * 'Member>

type Class =
    { ClassName: GenericName
      Body: Statement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier list
      Members: MemberSet<ClassMember>
      PrimaryCtor: (Access * Ctor) option
      SelfIdentifier: IdentifierStr
      SuperClass: FullIdentifier option }
and ClassMember = TypeOrMember<Class, InstanceMember>

type Interface =
    { InterfaceName: GenericName
      Members: MemberSet<InterfaceMember>
      SuperInterfaces: FullIdentifier list }
and InterfaceMember = TypeOrMember<Interface, AbstractMember>

type Module<'Type> =
    { Body: Statement list
      ModuleName: SimpleName
      Members: MemberSet<TypeOrMember<'Type, StaticMember>> }

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module
and Module = Module<TypeDef>

type EntryPoint =
    { Body: Statement list
      Origin: Position
      Parameters: ExpParam list }
