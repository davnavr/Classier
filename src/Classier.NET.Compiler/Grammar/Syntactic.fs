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

/// Contains the parser for the language that transforms tokens into a concrete syntax tree.
module Classier.NET.Compiler.Grammar.Syntactic

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Grammar.Lexical
open Classier.NET.Compiler.Node
open Classier.NET.Compiler.Parser

type Node =
    /// Contains the imported namespaces and the nodes in the source file.
    | CompilationUnit of seq<Identifier> * seq<Node>
    /// Indicates that the node contains unexpected tokens.
    | Skipped
    | Block of seq<Node>
    | ClassDef
    | Comment of string
    | CtorDef
    | Expression
    | FieldDef of Identifier
    | Identifier of Identifier
    /// Represents an integer literal of a specified kind (decimal/hexadecimal/binary).
    | IntLit of int * TokenType
    | Newline
    | MethodCall of Identifier
    | MethodDef of Identifier
    | Statement // of Expression
    | Whitespace
and Identifier = seq<string>

let parser: Parser<Token, Node> =
    Parser (fun tokens -> 
        let nodes =
            (Item.ofSeq tokens, None)
            |> Seq.unfold (fun (item, unknown) -> // TODO: Have parser function be included in state tuple?
                match item with
                | Some _ ->
                    None
                | None -> None)
        invalidOp "Not implemented")
