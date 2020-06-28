namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Identifier

type GenAccess =
    | GenPublic
    | GenInternal
    | GenProtected
    | GenPrivate

type GenInterfaceMember<'Type> =
    | InterfaceMthd
    | InterfaceProp
type GenInterface<'Type> =
    { InterfaceName: Identifier<GenericParam>
      Members: ImmutableSortedSet<Grammar.TypeOrMember<GenInterface<'Type>, GenInterfaceMember<'Type>>>
      SuperInterfaces: ImmutableSortedSet<InterfaceInheritance<'Type>>
      Syntax: Grammar.Interface }
and InterfaceInheritance<'Type> =
    | DefinedInterface of GenInterface<'Type>
    | ExternInterface of Globals.EInterface

type GenMemberSet<'Type, 'Member> =
    ImmutableSortedSet<GenAccess * Grammar.TypeOrMember<'Type, 'Member>>

type GenClassMember<'Type> =
    | ClassCtor of GenCtor<'Type>
    | ClassMthd
    | ClassProp
type GenClass<'Type> =
    { ClassName: Identifier<GenericParam>
      Interfaces: ImmutableSortedSet<InterfaceInheritance<GenInterface<'Type>>>
      Members: GenMemberSet<GenClass<'Type>, GenClassMember<'Type>>
      SuperClass: ClassInheritance<'Type>
      Syntax: Grammar.Class }
and ClassInheritance<'Type> =
    | DefinedClass of GenClass<'Type>
    | ExternClass of Globals.EClass

type GenModuleMember<'Type> =
    | ModuleFunc
    | ModuleOper
type GenModule<'Type, 'Nested> =
    { Members: GenMemberSet<'Nested, GenModuleMember<'Type>>
      ModuleName: IdentifierStr
      Syntax: Grammar.Module }

type GenType =
    | GenClass of GenClass<TypeRef>
    | GenInterface of GenInterface<TypeRef>
    | GenModule of GenModule<TypeRef, GenType>
and TypeRef =
    | RDefined of GenType
    | RExtern of Globals.EType
    | RFunc of TypeRef * TypeRef
    | RPrimitive of TypeSystem.PrimitiveType
    | RTuple of TypeRef list
