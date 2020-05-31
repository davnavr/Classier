namespace Classier.NET.Compiler

open System.Collections.Immutable
open FParsec

// TODO: Symbol table should only be filled with classes, modules, interfaces, methods, functions, etc. & should ignore local vars.
type SymbolOrigin =
    /// Indicates that the symbol originated from some other source, such as a *.dll file.
    | External of string
    | SourceCode of Position

type Symbol =
    | GParam
    | Member
    | Namespace
    | Type

type ResolvedSymbol =
    { FullName: Identifier list
      Origin: SymbolOrigin
      Symbol: Symbol }

type UnknownSymbol =
    { Name: Identifier
      PossibleTypes: Symbol list
      PossibleParents: ResolvedSymbol list }

type SymbolTable =
    { Namespaces: ImmutableSortedDictionary<string list, ResolvedSymbol list> }

module SymbolTable =
    let empty =
        { Namespaces = ImmutableSortedDictionary.Empty } : SymbolTable

    let addNamespace ns table =
        let namespaces =
            if table.Namespaces.ContainsKey(ns)
            then table.Namespaces
            else table.Namespaces.Add(ns, List.empty)
        { table with Namespaces = namespaces }
