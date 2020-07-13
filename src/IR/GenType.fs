[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.IR.GenType

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar.Ast

let name gtype =
    match gtype with
    | GenClass gclass -> gclass.ClassName
    | GenInterface gintf -> gintf.InterfaceName
    | GenModule gmodl -> Identifier.ofStr gmodl.ModuleName

let syntax gtype =
    match gtype with
    | GenClass gclass -> Class gclass.Syntax
    | GenInterface gintf -> Interface gintf.Syntax
    | GenModule gmodl -> Module gmodl.Syntax

let gclass members syntax =
    { ClassName =
        GenName.ofIdentifier syntax.ClassName.Identifier
      Interfaces = InterfaceSet.empty
      Members = members
      PrimaryCtor =
        { Body = GenBody.empty()
          Parameters = ImmutableList.Empty
          Syntax = syntax.PrimaryCtor }
      SuperClass = None
      Syntax = syntax }

let ginterface members syntax =
    { InterfaceName =
        GenName.ofIdentifier syntax.InterfaceName.Identifier
      Members = members
      SuperInterfaces = InterfaceSet.empty
      Syntax = syntax }

let gmodule members syntax =
    { Members = members
      ModuleName = syntax.ModuleName.Identifier
      Syntax = syntax }
