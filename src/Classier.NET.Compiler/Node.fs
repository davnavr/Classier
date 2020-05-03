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

/// Contains the line number and line position.
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
      Content: string
      Position: LineInfo
      Value: 'Value }

let terminal content value pos: Node<'Value> =
    { Children = Seq.empty
      Content = content
      Position = pos
      Value = value }
