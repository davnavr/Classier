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

module Classier.NET.Compiler.NodeParsers

open FParsec

open Classier.NET.Compiler.SyntaxNode

type NodeListParser<'Value> = Parser<SyntaxNode<'Value> list, unit> // TODO: Use seq instead of list.

let node (node: 'Result -> LinePos -> SyntaxNode<'Value>) (parser: Parser<'Result, unit>) =
    getPosition
    .>>. parser
    |>> fun (pos, result) ->
        node result (LinePos pos)

let nodePair p: NodeListParser<'Value> =
    p |>> fun (r1, r2) -> [ r1; r2 ]

let strToken (str: string) (value: 'Value) =
    pstring str |> node (createToken value)

let charToken (c: char) (value: 'Value) =
    strToken (string c) value

let nodesOpt p: NodeListParser<'Value> =
    opt p
    |>> fun r ->
        match r with
        | Some _ -> r.Value
        | _ -> List.empty
