[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.ToSource.GenType

open Classier.NET.Compiler

let name gtype =
    match gtype with
    | GenClass gclass -> gclass.ClassName
    | GenInterface gintf -> gintf.InterfaceName
    | GenModule gmodl -> Identifier.ofStr gmodl.ModuleName

let syntax gtype =
    match gtype with
    | GenClass gclass -> Grammar.Class gclass.Syntax
    | GenInterface gintf -> Grammar.Interface gintf.Syntax
    | GenModule gmodl -> Grammar.Module gmodl.Syntax

let gclass interfaces members (syntax: Grammar.Class) =
    { ClassName =
        syntax.ClassName.Identifier
        |> GenName.ofIdentifier
      Interfaces = interfaces
      Members = members
      SuperClass = None
      Syntax = syntax }
