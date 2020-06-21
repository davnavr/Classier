module Classier.NET.Compiler.Grammar.TypeDef

open System.Collections.Immutable // TODO: Remove
open Classier.NET.Compiler.Identifier

//// TODO: Use the word empty rather than placeholder.
let placeholderClass name =
    Class
        {| ClassName = name
           Body = List.empty
           Inheritance = ClassInheritance.Sealed
           Interfaces = List.empty
           Members = ImmutableSortedSet.Empty // TODO: These need the comparer!
           PrimaryCtor = None
           SelfIdentifier = IdentifierStr "this"
           SuperClass = None |}

let placeholderInterface name =
    Interface
        {| InterfaceName = name
           Members = ImmutableSortedSet.Empty
           SuperInterfaces = List.empty |}

let placeholderModule name =
    Module
        {| Body = List.empty
           ModuleName = name
           Members = ImmutableSortedSet.Empty |}

let getMembers tdef =
    match tdef with
    | Class cdef -> cdef.Members
    | Interface idef -> idef.Members
    | Module mdef -> mdef.Members
