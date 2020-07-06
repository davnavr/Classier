namespace Classier.NET.Compiler.Grammar

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

type MemberList<'Type, 'Member> = (Access * TypeOrMember<'Type, 'Member>) list

type Class =
    { ClassName: GenericName
      Body: PStatement list
      Inheritance: ClassInheritance
      Interfaces: FullIdentifier<TypeArg> list
      Members: MemberList<Class, InstanceMember>
      PrimaryCtor: Access * InfParam list * Expression list
      SelfIdentifier: IdentifierStr option
      SuperClass: FullIdentifier<TypeArg> option }

    override this.ToString() =
        sprintf "%Oclass %O" this.Inheritance this.ClassName

type Interface =
    { InterfaceName: GenericName
      Members: MemberList<Interface, AbstractMember>
      SuperInterfaces: FullIdentifier<TypeArg> list }

type Module<'Type> =
    { ModuleName: SimpleName
      Members: MemberList<'Type, StaticMember> }

type TypeDef =
    | Class of Class
    | Interface of Interface
    | Module of Module<TypeDef>

type EntryPoint =
    { Arguments: ExpParam list
      Body: PStatement list
      Origin: FParsec.Position }
