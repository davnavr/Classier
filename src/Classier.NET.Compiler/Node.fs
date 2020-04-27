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

module Classier.NET.Compiler.Node

open System

open Classier.NET.Compiler.Item
open Classier.NET.Compiler.Tokenizer
open Classier.NET.Compiler.Matching

type Node<'Token, 'Value> = seq<'Token> * 'Value

type NodeParser<'Token, 'Value> =
    NodeParser of (Item<'Token> option -> Item<'Token> option * Node<'Token, 'Value> option)

let toString (contentMap: 'Token -> string) ((tokens, _): Node<'Token, 'Value>) =
    tokens
    |> Seq.map contentMap
    |> String.Concat

let parserOfMatch (value: seq<'Token> -> 'Value) (childNodes: (Item<'Token> -> seq<Node<'Token, 'Value>>) option) (f: MatchFunc<'Token>) =
    NodeParser (fun item ->
        match evaluateMatch f item with
        | Success (tokens, nextItem) ->
            let children =
                match childNodes with
                | Some nodeGen -> nodeGen item.Value
                | None -> Seq.empty

            nextItem, Some (tokens, value tokens)
        | Failure _ ->
            item, None)

let parseNode item (NodeParser parser: NodeParser<'Token, 'Value>) = parser(item)

let parseAny item (parsers: seq<NodeParser<'Token, 'Value>>) =
    let node =
        parsers
        |> Seq.map (parseNode item)
        |> Seq.tryFind (fun (_, node) -> node.IsSome)

    match node with
    | Some actualNode -> actualNode
    | None -> item, None
