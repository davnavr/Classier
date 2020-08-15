[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.ResolvedType

open Classier.NET.Compiler
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
                |> GenClass
                |> ResolvedType.Named
                |> acc
    inner id

let rec isA tbase tderived =
    match (tbase, tderived) with
    | (ArrayType ibase, ArrayType iderived) ->
        isA ibase iderived
    | (Named tbase, Named tderived) ->
        invalidOp "bad"
    | (Primitive p1, Primitive p2) -> p1 = p2
    | (Tuple (bhead, btail), Tuple(thead, ttail)) ->
        let rec inner btail ttail =
            match (btail, ttail) with
            | (bt :: bnext, tt :: tnext) when isA bt tt ->
                inner bnext tnext
            | ([], []) -> true
            | _ -> false
        isA bhead thead && inner btail ttail
    | _ -> false
