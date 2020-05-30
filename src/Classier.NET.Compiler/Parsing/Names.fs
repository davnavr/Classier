// Copyright (c) 2020 NAME HERE
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

open System

type TypeName =
    | Identifier of Identifier list
    | Inferred
    | Tuple of TypeName list
    | Union of TypeName list

    override this.ToString() =
        match this with
        | Identifier names -> String.Join('.', names)
        | Inferred -> "_"
        | Tuple types ->
            String.Join(", ", types)
            |> sprintf "(%s)"
        | Union options -> String.Join(" | ", options)
and Identifier =
    { Name: string
      GenericArgs: GenericArg list }

    override this.ToString() =
        match this.GenericArgs with
        | [] -> this.Name
        | _ ->
            let gargs = String.Join(", ", this.GenericArgs)
            sprintf "%s<%s>" this.Name gargs
and GenericArg = TypeName

module Identifier =
    let ofStrings names =
        names
        |> List.map (fun name ->
            { Name = name
              GenericArgs = [] })
