namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Identifier

type GenParam<'Type> =
    { Name: IdentifierStr option
      Type: GlobalType<'Type> }

type GenParamTuple<'Type> =
    private
    | GenParamTuple of ImmutableList<GenParam<'Type>>

type GenParamList<'Type> =
    private
    | GenParamList of ImmutableList<GenParamTuple<'Type>>
