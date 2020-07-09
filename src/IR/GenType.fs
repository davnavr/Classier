[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.IR.GenType

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

let gclass interfaces members (syntax: Class) =
    { ClassName =
        syntax.ClassName.Identifier
        |> GenName.ofIdentifier
      Interfaces = interfaces
      Members = members
      SuperClass = None
      Syntax = syntax }
