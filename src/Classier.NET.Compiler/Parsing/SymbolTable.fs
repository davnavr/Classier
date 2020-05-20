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

type SymbolOrigin =
    /// Indicates that the symbol originated from some other source, such as a *.dll file.
    | External of string
    | SourceCode of Position

type Symbol =
    | GParam
    /// Local variable or parameter.
    | Local
    | Member
    | Namespace
    | Type // of Scope

type ResolvedSymbol =
    { FullName: string list
      Origin: SymbolOrigin
      Symbol: Symbol }

type UnknownSymbol =
    { Name: string
      PossibleTypes: Symbol list
      PossibleParents: ResolvedSymbol list }

type SymbolTable =
    { Namespaces: ImmutableSortedDictionary<string list, ResolvedSymbol list>
       }

module SymbolTable =
    let empty =
        { Namespaces = ImmutableSortedDictionary.Empty
          }

    let addNamespace ns table =
        let namespaces =
            if table.Namespaces.ContainsKey(ns)
            then table.Namespaces
            else table.Namespaces.Add(ns, List.empty)
        { table with Namespaces = namespaces }

