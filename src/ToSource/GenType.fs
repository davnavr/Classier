namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Identifier

type GenClassMember<'Type> =
    | ClassCtor of GenCtor<'Type>
    | ClassMthd
    | ClassProp

type GenMemberSet<'Type, 'Member> = ImmutableSortedSet<Grammar.TypeOrMember<'Type, 'Member>>

type GenClass<'Type> =
    { Access: Grammar.Access
      Interfaces: ImmutableSortedSet<unit> // Choice between a GenInterface or an external interface.
      Member: GenMemberSet<'Type, GenClassMember<'Type>>
      Name: Identifier<GenericParam>
      SuperClass: unit option // Choice between an external type or a GenType.
      Syntax: Grammar.Class }

type GenType =
    | GenClass
    | GenInterface
    | GenModule
