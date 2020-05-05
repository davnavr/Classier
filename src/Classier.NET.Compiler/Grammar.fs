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

open Classier.NET.Compiler.NodeParsers
open Classier.NET.Compiler.SyntaxNode

type NodeValue =
    | CompilationUnit of
        {| Definition: SyntaxNode<NodeValue>
           Imports: seq<SyntaxNode<NodeValue>>
           Namespace: SyntaxNode<NodeValue> option |}
    | AccessModifier
    | Block
    | ClassDef
    | Colon
    | Comment
    | FieldDef
    | Identifier
    | IdentifierChain
    | Keyword
    | LCurlyBracket
    | ModuleDef
    | NamespaceDef
    | Newline
    | OpAssign
    | Period
    | RCurlyBracket
    | UseStatement
    | Whitespace

let parser: Parser<SyntaxNode<NodeValue>, unit> =
    let pIgnored allowMultiline =
        choice
            [
                // TODO: Move these inside of here?
                anyOf [ ' '; '\t' ]
                |> many1Chars
                |> pnode (SyntaxNode.createToken Whitespace)
                <?> "whitespace";

                // pMlCommentOneLine

                if allowMultiline then
                    unicodeNewline
                    |> pnode (fun c pos ->
                        { Content = Token (string c)
                          Position = pos.NextLine
                          Value = Newline });

                    pstring "//" .>>. restOfLine false
                    |> pnode (fun (str1, str2) ->
                        SyntaxNode.createToken Comment (str1 + str2));

                    // pMlComment
            ]

    let pKeyword keyword =
        pstring keyword
        |> pnode (SyntaxNode.createToken Keyword)
        .>>. pIgnored false
        |> pnodePair

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
        |> pnode (SyntaxNode.createToken Identifier)

    let pIdentifierChain =
        many
            (pIdentifier
            .>>. (pIgnored false |> opt)
            .>>. (pstring "." |> pnode (SyntaxNode.createToken Period))
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
        |> pnode (SyntaxNode.createNode IdentifierChain)

    let pIdentifierStatement keyword value =
        pKeyword keyword
        .>>. pIdentifierChain
        |>> fun (word, identifier) -> List.append word [identifier]
        |> pnode (SyntaxNode.createNode value)

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
        |> pnode (SyntaxNode.createToken AccessModifier)
        .>>. pIgnored false
        |> pnodePair
        |> opt

    let pBlock (p: NodeListParser<NodeValue>) =
        many (pIgnored true)
        .>>. (pstring "{" |> pnode (SyntaxNode.createToken LCurlyBracket))
        .>>. p
        .>>. (pstring "}" |> pnode (SyntaxNode.createToken RCurlyBracket))
        |>> fun (((leading, lc), middle), rc) ->
            leading @ (lc :: middle @ [ rc ])

    let pexpression: Parser<SyntaxNode<NodeValue>, unit> =
        pstring "42"
        |> pnode (createToken Keyword) // Temporary

    let pFieldOrVar =
        pKeyword "let"
        .>>. pnodesOpt (pKeyword "mutable")
        .>>. pIdentifierChain
        .>>. opt (pIgnored false)
        .>>. (pcharNode ':' Colon)
        .>>. opt (pIgnored false)
        |>> (fun (((((wordl, wordm), name), sep1), colon), sep2) ->
            let trailing =
                [
                    name
                    if sep1.IsSome then
                        sep1.Value
                    colon
                    if sep2.IsSome then
                        sep2.Value
                ]
            wordl @ wordm @ trailing)
        .>>. pIdentifierChain
        .>>. opt (pIgnored false)
        .>>. (pcharNode '=' OpAssign)
        .>>. opt (pIgnored false)
        .>>. pexpression
        |>> (fun (((((def, valueType), sep1), eq), sep2), expr) ->
            let value =
                [
                    valueType
                    if sep1.IsSome then
                        sep1.Value
                    eq
                    if sep2.IsSome then
                        sep2.Value
                    expr
                ]
            List.append def value)
        |> pnode (createNode FieldDef)
        <?> "variable definition"

    let pMemberBlock instance =
        pBlock (
            choice
                [
                    pIgnored true;
                    pFieldOrVar;
                ]
            |> attempt
            |> many)

    let pClassDef nested =
        pAccessModifier nested
        .>>. opt (pKeyword "inheritable" |> attempt <|> pKeyword "abstract")
        .>>. opt (pKeyword "mutable")
        |>> (fun ((access, worde), wordm) ->
            [ access; worde; wordm ]
            |> List.choose id
            |> List.collect id)
        .>>. pKeyword "class"
        // NOTE: Implement primary constructors some other time.
        .>>. pIdentifier
        .>>. opt (pKeyword "extends" .>>. pIdentifierChain)
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
        .>>. pMemberBlock true
        |>> fun (header, body) -> header @ body
        |> pnode (SyntaxNode.createNode ClassDef)
        <?> "class definition"

    let pModuleDef nested =
        pAccessModifier nested
        .>>. pKeyword "module"
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
        .>>. pMemberBlock false
        |>> fun (header, body) -> header @ body
        |> pnode (SyntaxNode.createNode ModuleDef)
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
    |> pnode (fun (def, imports, ns, nodes) ->
        let compilationUnit =
            CompilationUnit
                {| Definition = def
                   Imports = imports
                   Namespace = ns |}
        SyntaxNode.createNode compilationUnit nodes)
