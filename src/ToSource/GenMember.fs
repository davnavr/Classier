namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable

type GenPrimaryCtor<'Type> =
    { Body: ImmutableList<unit>
      Parameters: GenParamTuple<'Type>
       }

type GenCtor<'Type> =
    { Parameters: GenParamTuple<'Type>
       }
