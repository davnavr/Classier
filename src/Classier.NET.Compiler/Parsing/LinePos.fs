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

namespace Classier.NET.Compiler.Parsing

/// Contains line number and line position.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type LinePos (lineNum: uint32, lineCol: uint32, name: string) =
    struct
        new (pos: FParsec.Position) = LinePos(uint32(pos.Line), uint32(pos.Column), pos.StreamName)
        member _.Line: uint32 = lineNum
        member _.Column: uint32 = lineCol
        member _.Name: string = name
    end
