[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.IR.GenExpression

open Classier.NET.Compiler
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.TypeSystem

let vtype =
    function
    | BoolLit ->
        ResolvedType.Primitive PrimitiveType.Boolean
    | ComplexExpr expr ->
        match expr with
        | CtorCall ctor ->
            ctor.Target
            |> DefinedOrExtern.map GenClass EClass
            |> ResolvedType.Named 
