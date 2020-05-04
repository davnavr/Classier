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

/// Contains line number and line position.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type LinePos (lineNum: uint32, linePos: uint32) =
    struct
        new (lineNum: int64, linePos: int64) = LinePos(uint32(lineNum), uint32(linePos))
        member _.LineNum: uint32 = lineNum
        member _.LineCol: uint32 = linePos
        member this.Advance chars = LinePos (this.LineNum, this.LineCol + chars)
        member this.Advance chars = this.Advance(uint32(chars))
        member this.NextLine = LinePos (this.LineNum + 1u, 0u)
    end
