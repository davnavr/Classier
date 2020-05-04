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
    | LCurlyBracket
    | ModuleDef
    | NamespaceDef
    | Newline
    | Period
    | RCurlyBracket
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

    let pIgnored allowMultiline =
        choice
            [
                // TODO: Move these inside of here?
                anyOf [ ' '; '\t' ]
                |> many1Chars
                |> parseNode (SyntaxNode.createToken Whitespace)
                <?> "whitespace";

                // pMlCommentOneLine

                if allowMultiline then
                    unicodeNewline
                    |> parseNode (fun c pos ->
                        { Content = Token (string c)
                          Position = pos.NextLine
                          Value = Newline });

                    pstring "//" .>>. restOfLine false
                    |> parseNode (fun (str1, str2) ->
                        SyntaxNode.createToken Comment (str1 + str2));

                    // pMlComment
            ]

    let parseKeyword keyword =
        pstring keyword
        |> parseNode (SyntaxNode.createToken Keyword)
        .>>. pIgnored false
        |>> fun (word, sep) -> [ word; sep ]

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
        .>>. pIdentifierChain
        |>> fun (word, identifier) -> List.append word [identifier]
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

    let pBlock (parser: Parser<SyntaxNode<NodeValue> list, unit>) =
        many (pIgnored true)
        .>>. (pstring "{" |> parseNode (SyntaxNode.createToken LCurlyBracket))
        .>>. parser
        .>>. (pstring "}" |> parseNode (SyntaxNode.createToken RCurlyBracket))
        |>> fun (((leading, lc), middle), rc) ->
            leading @ (lc :: middle @ [ rc ])

    let pClassDef nested: Parser<SyntaxNode<NodeValue>, unit> =
        pAccessModifier nested
        .>>. opt (parseKeyword "inheritable" |> attempt <|> parseKeyword "abstract")
        .>>. opt (parseKeyword "mutable")
        |>> (fun ((access, worde), wordm) ->
            [ access; worde; wordm ]
            |> List.choose id
            |> List.collect id)
        .>>. parseKeyword "class"
        // NOTE: Implement primary constructors some other time.
        .>>. pIdentifier
        .>>. opt (parseKeyword "extends" .>>. pIdentifierChain)
        |>> (fun (((modifiers, wordc), name), extend) ->
            [
                modifiers
                wordc
                [ name ]
                match extend with
                | Some (wordex, supername) ->
                    List.append wordex [ supername ]
                | _ -> List.empty
            ]
            |> List.collect id)
        .>>. pBlock (
                choice
                    [
                        pIgnored true
                    ]
                |> attempt
                |> many)
        |>> fun (header, body) -> header @ body
        |> parseNode (SyntaxNode.createNode ClassDef)
        <?> "class definition"

    let pModuleDef nested: Parser<SyntaxNode<NodeValue>, unit> =
        pAccessModifier nested
        .>>. parseKeyword "module"
        .>>. pIdentifier
        |>> (fun ((access, wordm), name) ->
            [
                match access with
                | Some _ -> access.Value
                | _ -> List.empty
                wordm
                [ name ]
            ]
            |> List.collect id)
        .>>. pBlock (
                choice
                    [
                        pIgnored true
                    ]
                |> attempt
                |> many)
        |>> fun (header, body) -> header @ body
        |> parseNode (SyntaxNode.createNode ModuleDef)
        <?> "module definition"

    pUseStatements
    .>>. opt (pIdentifierStatement "namespace" NamespaceDef <?> "namespace definition")
    .>>. pUseStatements
    .>>. (pClassDef false |> attempt <|> pModuleDef false) // NOTE: Use a choice here when another type of def is added.
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
