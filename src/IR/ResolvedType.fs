[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.ResolvedType

open Classier.NET.Compiler
open Classier.NET.Compiler.Extern
open Classier.NET.Compiler.TypeSystem

let ofExpr =
    let rec inner acc =
        let primitive = ResolvedType.Primitive >> acc
        function
        | BoolLit -> primitive PrimitiveType.Boolean
        | StrLit _ -> primitive PrimitiveType.String
        | ComplexExpr expr ->
            match expr with
            | CtorCall ctor ->
                ctor.Target
                |> DefinedOrExtern.map GenClass EClass
                |> ResolvedType.Named
                |> acc
    inner id
