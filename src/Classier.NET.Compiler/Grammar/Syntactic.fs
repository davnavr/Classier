// Copyright (c) 2020 David Navarro
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

open System
open System.Collections.Generic

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Grammar.Lexical
open Classier.NET.Compiler.Node
open Classier.NET.Compiler.Matching
open Classier.NET.Compiler.Parser

[<RequireQualifiedAccess>]
type Node =
    /// Contains the imported namespaces and the nodes in the source file.
    | CompilationUnit of {| Namespaces: seq<Identifier>; TypeDef: CSTNode |} * seq<CSTNode> // TODO: Is CSTNode necessary?
    /// Indicates that the node contains unexpected tokens.
    | Skipped
    | Block of seq<CSTNode>
    | ClassDef of seq<CSTNode>
    | Comment of string
    | CtorDef
    | Expression
    | FieldDef of Identifier
    | Identifier of Identifier
    /// Represents an integer literal of a specified kind (decimal/hexadecimal/binary).
    | IntLit of int * TokenType
    | MethodCall of Identifier // * seq<Expression>
    | MethodDef of Identifier * seq<Parameter>
    | NewLine
    | ParamDef of Parameter
    | Statement // of Expression
    | UseStatement of Identifier * seq<CSTNode>
    | Whitespace

    member this.Nodes =
        match this with
        | CompilationUnit (_, nodes) -> nodes
        | Block (nodes) -> nodes
        | _ -> Seq.empty

and CSTNode = Node<Token, Node>
and Identifier = seq<string>
and Parameter = { Type: Identifier; Name: Identifier }

let parser: Parser<Token, Node> =
    Parser (fun tokens ->
        let takeTokensOfType t start =
            start
            |> Item.takeValsWhile (fun (token: Token, _) -> token.Type = t)

        let nodes =
            seq {
                let startItem = Item.ofSeq tokens
                let mutable currentItem = startItem

                while currentItem.IsSome do
                    let currentToken = currentItem.Value.Value
                    match currentToken.Type with
                    | TokenType.Whitespace ->
                        yield
                            currentItem
                            |> takeTokensOfType TokenType.Whitespace,
                            Node.Whitespace
                    | TokenType.NewLine ->
                        yield Seq.singleton currentToken, Node.NewLine
                    | TokenType.Identifier -> invalidOp "Cannot parse identifiers yet"
                    | _ -> ()
            }

        let ns = Seq.empty
        let typeDef = Seq.empty, Node.Skipped

        tokens, Node.CompilationUnit ({| Namespaces = ns; TypeDef = typeDef |}, nodes))
