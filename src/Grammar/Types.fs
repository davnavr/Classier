namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

type ClassInheritance =
    | MustInherit
    | CanInherit
    | Sealed

    override this.ToString() =
        match this with
        | MustInherit -> "abstract "
        | CanInherit -> "inheritable "
        | _ -> ""

type TypeOrMember<'Type, 'Member> =
    | Type of 'Type
    | Member of 'Member

type Class =
    { ClassName: GenericName
      Body: Statement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier list
      Members: ImmutableList<Access * ClassMember>
      PrimaryCtor: Access * InfParam list * Expression list
      SelfIdentifier: IdentifierStr option
      SuperClass: FullIdentifier option }

    override this.ToString() =
        sprintf
            "%sclass %s"
            (string this.Inheritance)
            (string this.ClassName)
and ClassMember = TypeOrMember<Class, InstanceMember>

type Interface =
    { InterfaceName: GenericName
      Members: (Access * InterfaceMember) list
      SuperInterfaces: FullIdentifier list }
and InterfaceMember = TypeOrMember<Interface, AbstractMember>

type Module<'Type> =
    { Body: Statement list
      ModuleName: SimpleName
      Members: ImmutableList<Access * TypeOrMember<'Type, StaticMember>> }

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module
and Module = Module<TypeDef>

type EntryPoint =
    { Body: Statement list
      Origin: Position
      Parameters: ExpParam list }
