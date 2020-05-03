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

/// Contains line number and line position.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type LineInfo (lineNum: uint32, linePos: uint32) =
    struct
        new (lineNum: int64, linePos: int64) = LineInfo(lineNum, linePos)
        member _.LineNum: uint32 = lineNum
        member _.LinePos: uint32 = linePos
        member this.Advance chars = LineInfo (this.LineNum, this.LinePos + chars)
        member this.Advance chars = this.Advance(uint32(chars))
        member this.NextLine = LineInfo (this.LineNum + 1u, 0u)
    end

type Node<'Value> =
    { Children: seq<Node<'Value>>
      Content: string // TODO: Save memory by having TerminalNode that can only have strings and Node that can only have child nodes?
      Position: LineInfo
      Value: 'Value }

let terminal value content (oldPos: LineInfo): Node<'Value> =
    { Children = Seq.empty
      Content = content
      Position = oldPos.Advance(content.Length)
      Value = value }

let contentOf (nodes: seq<Node<'Value>>) =
    nodes
    |> Seq.map (fun node -> node.Content)
    |> System.String.Concat

let withChildren (value: 'Value) nodes (oldPos: LineInfo) =
    let content = contentOf nodes
    { Children = nodes
      Content = contentOf nodes
      Position = oldPos.Advance(content.Length)
      Value = value }
