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

module rec Classier.NET.Compiler.SyntaxNode

type SyntaxNode<'Value> =
    { Content: NodeOrToken<'Value>
      Position: LinePos
      Value: 'Value }
    with
        member this.Children =
            match this.Content with
            | Node children -> children
            | _ -> Seq.empty
        member this.Length =
            match this.Content with
            | Node children -> lengthOf children
            | Token content -> content.Length
        /// Returns the content of the node.
        override this.ToString() =
            match this.Content with
            | Node children ->
                children
                |> Seq.map string
                |> System.String.Concat
            | Token content -> content

type NodeOrToken<'Value> =
    | Node of seq<SyntaxNode<'Value>>
    | Token of string

let private lengthOf (nodes: seq<SyntaxNode<'Value>>) =
    nodes
    |> Seq.sumBy (fun node -> node.Length)

let createToken value content (oldPos: LinePos): SyntaxNode<'Value> =
    { Content = Token content
      Position = oldPos.Advance(content.Length)
      Value = value }

let createNode value children (oldPos: LinePos): SyntaxNode<'Value> =
    { Content = Node children
      Position = oldPos.Advance(lengthOf children)
      Value = value }
