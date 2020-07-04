namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler.Globals
open Classier.NET.Compiler.Identifier

type GenParam<'GenType> =
    { Name: IdentifierStr option
      Type: GlobalType<TypeRef<'GenType>> }

type GenParamTuple<'GenType> =
    | GenParamTuple of ImmutableList<GenParam<'GenType>>

type GenParamList<'GenType> =
    | GenParamList of ImmutableList<GenParamTuple<'GenType>>

module GenParam =
    let tupleTypes (GenParamTuple ptuple) =
        ptuple
        |> Seq.map (fun p -> p.Type)
        |> List.ofSeq
