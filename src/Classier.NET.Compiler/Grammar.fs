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

module Classier.NET.Compiler.Grammar

open FParsec

open Classier.NET.Compiler.SyntaxNode

type NodeValue =
    | CompilationUnit of
        {| Definition: SyntaxNode<NodeValue>
           Imports: seq<SyntaxNode<NodeValue>>
           Namespace: SyntaxNode<NodeValue> option |}
    | AccessModifier
    | Block
    | ClassDef
    | Comment
    | Identifier
    | IdentifierChain
    | Keyword
    | ModuleDef
    | NamespaceStatement
    | Newline
    | Period
    | UseStatement
    | Whitespace

let parser: Parser<SyntaxNode<NodeValue>, unit> =
    let parseNode (node: 'Result -> LinePos -> SyntaxNode<NodeValue>) (parser: Parser<'Result, unit>) =
        pipe2
            parser
            (fun stream ->
                LinePos (stream.Position.Line, stream.Position.Index)
                |> Reply)
            node

    let parseKeyword keyword =
        pstring keyword
        |> parseNode (SyntaxNode.createToken Keyword)

    let pWhitespace =
        anyOf [ ' '; '\t' ]
        |> many1Chars
        |> parseNode (SyntaxNode.createToken Whitespace)

    let pNewline =
        unicodeNewline
        |> parseNode (fun c pos ->
            { Content = Token (string c)
              Position = pos.NextLine
              Value = Newline })

    let pSlComment =
        pstring "//" .>>. restOfLine false
        |> parseNode (fun (str1, str2) ->
            SyntaxNode.createToken Comment (str1 + str2))

    let pIgnored allowMultiline =
        choice
            [
                // TODO: Move these inside of here?
                pWhitespace
                pNewline
                // pMlCommentOneLine
                if allowMultiline then
                    pSlComment
                    // pMlComment
            ]

    let pIdentifier =
        let palphabet =
            List.append [ 'a'..'z' ] [ 'A'..'Z' ]
            |> anyOf
        palphabet
        .>>. many (palphabet <|> digit)
        |>> fun (c, rest) ->
            c :: rest
            |> Array.ofList
            |> System.String
        |> parseNode (SyntaxNode.createToken Identifier)

    let pIdentifierChain =
        many
            (pIdentifier
            .>>. (pIgnored false |> opt)
            .>>. (pstring "." |> parseNode (SyntaxNode.createToken Period))
            .>>. (pIgnored false |> opt)
            |> attempt)
        .>>. pIdentifier
        |>> fun (leading, last) ->
            Seq.append
                (leading
                |> Seq.collect (fun (((id, sep1), per), sep2) ->
                    [
                        id
                        if sep1.IsSome then
                            sep1.Value
                        per
                        if sep2.IsSome then
                            sep2.Value
                    ]))
                [ last ]
        |> parseNode (SyntaxNode.createNode IdentifierChain)

    let pIdentifierStatement keyword value =
        parseKeyword keyword
        .>>. pIgnored false
        .>>. pIdentifierChain
        |>> fun ((word, sep), identifier) -> [ word; sep; identifier ]
        |> parseNode (SyntaxNode.createNode value)

    let pUseStatements =
        choice
            [
                pIgnored true
                pIdentifierStatement "use" UseStatement
            ]
        |> many

    // Includes trailing whitespace
    let pAccessModifier full =
        [
            "public";
            "internal";
            if full then
                "protected";
                "private";
        ]
        |> Seq.map pstring
        |> choice
        |> parseNode (SyntaxNode.createToken AccessModifier)
        .>>. many (pIgnored false)
        |>> List.Cons
        |> opt

    let pClassDef nested: Parser<SyntaxNode<NodeValue>, unit> =
        pAccessModifier nested
        .>>. parseKeyword "class"
        fail "not implemented"

    let pModuleDef nested: Parser<SyntaxNode<NodeValue>, unit> =
        pAccessModifier nested
        .>>. parseKeyword "module"
        fail "not implemented"

    pUseStatements
    .>>. opt (pIdentifierStatement "namespace" NamespaceStatement)
    .>>. pUseStatements
    .>>. (pClassDef false <|> pModuleDef false)
    .>>. many (pIgnored true)
    |>> fun ((((use1, ns), use2), classOrModule), trailing) ->
        let nodes =
            trailing
            |> List.append [ classOrModule ]
            |> List.append use2
            |> List.append (List.choose id [ ns ])
            |> List.append use1
        let imports =
            Seq.append use1 use2
            |> Seq.choose (fun node ->
                match node.Value with
                | UseStatement -> Some node
                | _ -> None)
        classOrModule, imports, ns, nodes
    |> parseNode (fun (def, imports, ns, nodes) ->
        let compilationUnit =
            CompilationUnit
                {| Definition = def
                   Imports = imports
                   Namespace = ns |}
        SyntaxNode.createNode compilationUnit nodes)
