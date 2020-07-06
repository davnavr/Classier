namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable
open Classier.NET.Compiler.AccessControl
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
      Body: PStatement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier<TypeArg> list
      Members: ImmutableList<Access * ClassMember>
      PrimaryCtor: Access * InfParam list * Expression list
      SelfIdentifier: IdentifierStr option
      SuperClass: FullIdentifier<TypeArg> option }

    override this.ToString() =
        sprintf "%Oclass %O" this.Inheritance this.ClassName
and ClassMember = TypeOrMember<Class, InstanceMember>

type Interface =
    { InterfaceName: GenericName
      Members: (Access * InterfaceMember) list
      SuperInterfaces: FullIdentifier<TypeArg> list }
and InterfaceMember = TypeOrMember<Interface, AbstractMember>

type Module<'Type> =
    { ModuleName: SimpleName
      Members: (Access * TypeOrMember<'Type, StaticMember>) list }

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module
and Module = Module<TypeDef>

type EntryPoint =
    { Arguments: ExpParam list
      Body: PStatement list
      Origin: FParsec.Position }
