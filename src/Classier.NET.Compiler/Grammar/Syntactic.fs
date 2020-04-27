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

/// Contains the parser for the language that transforms tokens into a concrete syntax tree.
module Classier.NET.Compiler.Grammar.Syntactic

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Grammar.Lexical
open Classier.NET.Compiler.Node
open Classier.NET.Compiler.Matching
open Classier.NET.Compiler.Parser

type Node =
    /// Contains the imported namespaces and the nodes in the source file.
    | CompilationUnit of {| Namespaces: seq<Identifier>; TypeDef: Node<Token, Node> |} * seq<Node<Token, Node>>
    /// Indicates that the node contains unexpected tokens.
    | Skipped
    | Block of seq<Node<Token, Node>>
    | ClassDef
    | Comment of string
    | CtorDef
    | Expression
    | FieldDef of Identifier
    | Identifier of Identifier
    /// Represents an integer literal of a specified kind (decimal/hexadecimal/binary).
    | IntLit of int * TokenType
    | MethodCall of Identifier
    | MethodDef of Identifier * seq<Parameter>
    | Newline
    | ParamDef of Parameter
    | Statement // of Expression
    | UseStatement of Identifier
    | Whitespace

    member this.Nodes =
        match this with
        | CompilationUnit (_, nodes) -> nodes
        | Block (nodes) -> nodes
        | _ -> Seq.empty

and Identifier = seq<string>
and Parameter = Identifier * Identifier

let parser: Parser<Token, Node> =
    Parser (fun tokens ->
        let matchTokenType t =
            (fun token -> token.Type = t)
            |> matchPredicate (string t)

        let nodeType t _ = t
        
        let parseWhitespace =
            matchTokenType TokenType.Whitespace
            |> matchMany
            |> parserOfMatch (nodeType Node.Whitespace) None

        /// The main parsing function.
        let entryPoint = null
        let nodes =
            (Item.ofSeq tokens, None, entryPoint)
            |> Seq.unfold (fun (item, unknown, pfunc) -> // TODO: Have parser function be included in state tuple?
                match item with
                | Some _ ->
                    None
                | None -> None)
            |> Seq.cache

        let ns = seq { invalidOp "Not implemented" }
        let typeDef = Seq.empty, Skipped

        tokens, CompilationUnit ({| Namespaces = ns; TypeDef = typeDef |}, nodes))
