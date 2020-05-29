// Copyright (c) 2020 David Navarro
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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

type ResolvedSymbol<'Name> =
    { FullName: 'Name list
      Origin: SymbolOrigin
      Symbol: Symbol }

type UnknownSymbol<'Name> =
    { Name: 'Name
      PossibleTypes: Symbol list
      PossibleParents: ResolvedSymbol<'Name> list }

type SymbolTable<'Name> =
    { Namespaces: ImmutableSortedDictionary<string list, ResolvedSymbol<'Name> list> }

module SymbolTable =
    let empty<'Name> =
        { Namespaces = ImmutableSortedDictionary.Empty } : SymbolTable<'Name>

    let addNamespace ns table =
        let namespaces =
            if table.Namespaces.ContainsKey(ns)
            then table.Namespaces
            else table.Namespaces.Add(ns, List.empty)
        { table with Namespaces = namespaces }
