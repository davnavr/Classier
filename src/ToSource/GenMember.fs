namespace Classier.NET.Compiler.ToSource

open System.Collections.Immutable

type GenPrimaryCtor<'GenType> =
    { Body: ImmutableList<unit>
      Parameters: GenParamTuple<'GenType>
       }

type GenCtor<'GenType> =
    { Parameters: GenParamTuple<'GenType>
       }
