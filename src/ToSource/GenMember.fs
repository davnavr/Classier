namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Identifier

type GenPrimaryCtor<'Type> =
    { Body: ImmutableList<unit>
      Parameters: GenParamTuple<'Type>
       }

type GenCtor<'Type> =
    { Parameters: GenParamTuple<'Type>
       }
