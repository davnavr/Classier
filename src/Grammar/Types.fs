namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Generic
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
    { ClassName: GenericName // TODO: Names for types and members should be Identifier<GenericParam> not Identifier<Generic>
      Body: PStatement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier<Generic> list
      Members: ImmutableList<Access * ClassMember>
      PrimaryCtor: Access * InfParam list * Expression list
      SelfIdentifier: IdentifierStr option
      SuperClass: FullIdentifier<Generic> option }

    override this.ToString() =
        sprintf "%Oclass %O" this.Inheritance this.ClassName
and ClassMember = TypeOrMember<Class, InstanceMember>

type Interface =
    { InterfaceName: GenericName
      Members: (Access * InterfaceMember) list
      SuperInterfaces: FullIdentifier<Generic> list }
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
    { Arguments: ExpParam
      Body: PStatement list
      Origin: FParsec.Position }
