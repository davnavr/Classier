﻿// Copyright (c) 2020 David Navarro
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

/// Turns tokens into a concrete syntax tree.
module Classier.NET.Compiler.Parsing

open System

open Classier.NET.Compiler.Lexing
open Classier.NET.Compiler.Matching

module Node =
    type Node<'Token, 'Value> =
        { Nodes: seq<Node<'Token, 'Value>>
          Tokens: seq<'Token>
          Value: 'Value }

open Node

type Parser<'Token, 'Value> = Parser of (seq<'Token> -> Node<'Token, 'Value>)
