module Classier.NET.Compiler.Grammar

open System
open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

type EntryPoint =
    { Body: Statement list
      Origin: Position
      Parameters: ExpParam list }

type ClassInheritance =
    | MustInherit
    | CanInherit
    | Sealed

/// TODO: Differentiate between abstract types and non-abstract types, and abstract members and non-abstract members.
type TypeDef<'Member> =
    | Class of
        {| ClassName: Name
           Body: Statement list
           Inheritance: ClassInheritance
           Interfaces: FullIdentifier list
           Members: ImmutableSortedSet<'Member>
           PrimaryCtor: (Access * Constructor) option
           SelfIdentifier: IdentifierStr
           SuperClass: FullIdentifier option |}
    | Interface of
        {| InterfaceName: Name
           Members: ImmutableSortedSet<'Member>
           SuperInterfaces: FullIdentifier list |}
    | Module of
        {| Body: Statement list
           ModuleName: Name
           Members: ImmutableSortedSet<'Member> |}

type TypeDef = TypeDef<Member>

module TypeDef =
    let placeholderClass name =
        Class
            {| ClassName = name
               Body = List.empty
               Inheritance = ClassInheritance.Sealed
               Interfaces = List.empty
               Members = MemberDef.emptyMemberSet
               PrimaryCtor = None
               SelfIdentifier = IdentifierStr "this"
               SuperClass = None |}

    let placeholderInterface name =
        Interface
            {| InterfaceName = name
               Members = MemberDef.emptyMemberSet
               SuperInterfaces = List.empty |}

    let placeholderModule name =
        Module
            {| Body = List.empty
               ModuleName = name
               Members = MemberDef.emptyMemberSet |}

    let getMembers tdef =
        match tdef with
        | Class cdef -> cdef.Members
        | Interface idef -> idef.Members
        | Module mdef -> mdef.Members

type CompilationUnit =
    { EntryPoint: EntryPoint option
      Namespace: FullIdentifier option
      Usings: FullIdentifier list
      Source: string
      Types: seq<Access * TypeDef> }
