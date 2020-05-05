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

type NodeListParser<'Value> = Parser<SyntaxNode<'Value> list, unit>

let pnode (node: 'Result -> LinePos -> SyntaxNode<'Value>) (parser: Parser<'Result, unit>) =
    pipe2
        parser
        (fun stream ->
            LinePos (stream.Position.Line, stream.Position.Index)
            |> Reply)
        node

let pnodePair p: NodeListParser<'Value> =
    p |>> fun (r1, r2) -> [ r1; r2 ]

let pcharNode (c: char) (value: 'Value) =
    pstring (string c)
    |> pnode (createToken value)

let pnodesOpt p: NodeListParser<'Value> =
    opt p
    |>> fun r ->
        match r with
        | Some _ -> r.Value
        | _ -> List.empty
