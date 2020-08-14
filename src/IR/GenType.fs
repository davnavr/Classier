[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.GenType

open System.Collections.Immutable

open Classier.NET.Compiler

open Classier.NET.Compiler.Grammar.Ast

let private mglobal clss intf modl gtype =
    match gtype with
    | GenGlobalClass gclass -> clss gclass
    | GenGlobalInterface gintf -> intf gintf
    | GenGlobalModule gmodl -> modl gmodl

let gname =
    mglobal
        (fun clss -> clss.ClassName)
        (fun intf -> intf.InterfaceName)
        (fun mdle -> Identifier.ofStr mdle.ModuleName)

let nname =
    function
    | GenNestedClass nclass -> nclass.ClassName
    | GenNestedInterface nintf -> nintf.InterfaceName
    | GenNestedModule nmdle -> Identifier.ofStr nmdle.ModuleName

let gsyntax =
    mglobal
        (GenMember.syntax >> Class)
        (GenMember.syntax >> Interface)
        (GenMember.syntax >> Module)

let clss parent members syntax =
    let info =
        { ClassName =
            GenName.ofIdentifier syntax.ClassName.Identifier
          Interfaces = InterfaceSet.empty
          Members = members
          Parent = parent
          PrimaryCtor = invalidOp "bad"
          SuperClass = None
          Syntax = syntax }
    let pctor =
        { Body = ImmutableList.Empty // TODO: How will members reference parent type if they have to be in the Members set of the parent? How to solve recursion?
          ParentClass = invalidOp "badder"
          Parameters = ImmutableList.Empty
          Syntax = syntax.PrimaryCtor }
    { ClassName =
        GenName.ofIdentifier syntax.ClassName.Identifier
      Interfaces = InterfaceSet.empty
      Members = members
      Parent = parent
      PrimaryCtor = invalidOp "bad"
      SuperClass = None
      Syntax = syntax }

let intf parent members syntax =
    { InterfaceName =
        GenName.ofIdentifier syntax.InterfaceName.Identifier
      Members = members
      Parent = parent
      SuperInterfaces = InterfaceSet.empty
      Syntax = syntax }

let mdle parent members syntax =
    { Members = members
      ModuleName = syntax.ModuleName.Identifier
      Parent = parent
      Syntax = syntax }
