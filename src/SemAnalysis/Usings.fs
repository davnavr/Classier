namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable

type internal Usings =
    private
    | Usings of ImmutableSortedSet<unit>

module internal Usings =
    let empty = Usings ImmutableSortedSet.Empty
