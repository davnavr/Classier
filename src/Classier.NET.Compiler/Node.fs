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

/// Contains the zero-based line number and line position.
type LineInfo =
    { LineNum: int
      LinePos: int }
    with
        static member Default = { LineNum = 0; LinePos = 0; }
        static member (+) (info, pos) =
            { LineNum = info.LineNum
              LinePos = info.LinePos + pos }

        member this.NextLine =
            { LineNum = this.LineNum + 1
              LinePos = 0 }

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
