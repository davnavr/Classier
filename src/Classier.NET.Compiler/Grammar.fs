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
    | ClassDef // TODO: Add anonymous records to some to store information.
    | Colon
    | Comma
    | Comment
    | FieldDef
    | Identifier
    | IdentifierChain
    | Keyword
    | LCurlyBracket
    | LParen
    | MethodDef
    | MethodHeader
    | ModuleDef
    | NamespaceDef
    | Newline
    | OpAssign
    | Param // of {| Name: string |}
    | ParamTuple // of seq<SyntaxNode<NodeValue>>
    | ParamSet
    | Period
    | RCurlyBracket
    | RParen
    | UseStatement
    | Whitespace

let parser: Parser<SyntaxNode<NodeValue>, unit> =
    let pIgnored allowMultiline =
        choice
            [
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

    let pKeywordOpt keyword = pnodesOpt (pKeyword keyword)

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
            .>>. pcharToken '.' Period
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

    // Optional, and includes trailing whitespace
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
        |> pnodesOpt
        <?> "access modifier"

    let pBlock (p: NodeListParser<NodeValue>) =
        many (pIgnored true)
        .>>. pcharToken '{' LCurlyBracket
        .>>. p
        .>>. pcharToken '}' RCurlyBracket
        |>> fun (((leading, lc), middle), rc) ->
            leading @ (lc :: middle @ [ rc ])
        |> pnode (createNode Block)

    let pexpression: Parser<SyntaxNode<NodeValue>, unit> =
        pstring "42"
        |> pnode (createToken Keyword) // Temporary

    let pNameAndType =
        pIdentifier
        .>>. opt (pIgnored false)
        .>>. pcharToken ':' Colon
        .>>. opt (pIgnored false)
        .>>. pIdentifierChain
        |>> (fun ((((name, sep1), col), sep2), varType) ->
            [
                name
                if sep1.IsSome then
                    sep1.Value
                col
                if sep2.IsSome then
                    sep2.Value
                varType
            ])

    let pFieldOrVar =
        pKeyword "let"
        // TODO: Add modifiers "mutable"
        .>>. pNameAndType
        |>> (fun (wordl, names) -> wordl @ names)
        .>>. opt (pIgnored false)
        .>>. pcharToken '=' OpAssign
        .>>. opt (pIgnored false)
        .>>. pexpression
        |>> (fun (((((def), sep1), eq), sep2), expr) ->
            let value =
                [
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

    let pFuncKeywords =
        pAccessModifier true
        .>>. pKeyword "def"
        // TODO: Add modifiers "mutator", "abstract", and "inline"
        |>> fun (acc, def) ->
            List.collect id [ acc; def ]

    let pFuncParamTuple =
        let pParam =
            pNameAndType
            |> pnode (createNode Param)

        pcharToken '(' LParen
        .>>. opt (pIgnored false)
        .>>. pnodesOpt (
            many (
                pParam
                .>>. opt (pIgnored false)
                .>>. pcharToken ',' Comma
                .>>. opt (pIgnored false)
                |>> (fun (((param, sep1), comma), sep2) ->
                    [
                        param
                        if sep1.IsSome then
                            sep1.Value
                        comma
                        if sep2.IsSome then
                            sep2.Value
                    ])
                |> attempt)
            |>> List.collect id
            .>>. pParam
            |>> fun (rest, last) -> rest @ [ last ])
        .>>. opt (pIgnored false)
        .>>. pcharToken ')' RParen
        |>> (fun ((((lparen, sep1), parameters), sep2), rparen) ->
            let left =
                [
                    lparen
                    if sep1.IsSome then
                        sep1.Value
                ]
            let right =
                [
                    rparen
                    if sep2.IsSome then
                        sep2.Value
                ]
            left @ parameters @ right)
        |> pnode (createNode ParamTuple)
        <?> "parameters"

    let pFuncParamList = // TODO: Fix, parses one too many parameter tuples
        many (
            pFuncParamTuple
            .>>. opt (pIgnored false)
            |>> fun (tuple, sep) ->
                [
                    tuple
                    if sep.IsSome then
                        sep.Value
                ]
            |> attempt)
        |>> List.collect id
        .>>. pFuncParamTuple
        |>> (fun (rest, last) -> rest @ [ last ])
        |> pnode (createNode ParamSet)
        <?> "parameter list"

    let pFuncBody =
        pBlock (choice [ pIgnored true ] |> attempt |> many) // Temporary

    let pMemberBlock instance: Parser<SyntaxNode<NodeValue>, unit> =
        pBlock (
            choice
                [
                    pIgnored true
                    pFieldOrVar
                    if instance then
                        pFuncKeywords
                        .>>. (pIdentifier <?> "self identifier")
                        .>>. opt (pIgnored false)
                        .>>. pcharToken '.' Period
                        .>>. opt (pIgnored false)
                        .>>. pIdentifier
                        .>>. pIgnored false
                        |>> (fun ((((((words, self), sep1), per), sep2), name), sep3) ->
                            let header = 
                                [
                                    self
                                    if sep1.IsSome then
                                        sep1.Value
                                    per
                                    if sep2.IsSome then
                                        sep2.Value
                                    name
                                    sep3
                                ]
                            words @ header)
                        |> pnode (createNode MethodHeader)
                        .>>. pFuncParamList
                        |> pnodePair // TODO: Replace this line, add method body
                        |> pnode (createNode MethodDef)
                        <?> "method definition";
                ]
            |> attempt
            |> many)

    let pClassDef nested =
        pAccessModifier nested
        .>>. pnodesOpt (
            pnodesOpt (
                (pKeyword "inheritable" |> attempt <|> pKeyword "abstract")
                .>>. pIgnored false
                |>> fun (impl, sep) -> impl @ [ sep ] )
            .>>. pnodesOpt (
                pKeywordOpt "mutable"
                .>>. pIgnored false
                |>> fun (mut, sep) -> mut @ [ sep ])
            |>> (fun (impl, mut) -> impl @ mut))
        |>> (fun (access, modifiers) -> access @ modifiers)
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
        |>> fun (header, body) -> header @ [ body ]
        |> pnode (SyntaxNode.createNode ClassDef)
        <?> "class definition"

    let pModuleDef nested =
        pAccessModifier nested
        .>>. pKeyword "module"
        .>>. pIdentifier
        |>> (fun ((access, wordm), name) ->
            List.collect id [ access; wordm; [ name ] ])
        .>>. pMemberBlock false
        |>> fun (header, body) -> header @ [ body ]
        |> pnode (SyntaxNode.createNode ModuleDef)
        <?> "module definition"

    pUseStatements
    .>>. opt (pIdentifierStatement "namespace" NamespaceDef <?> "namespace definition")
    .>>. pUseStatements
    .>>. (pClassDef false) // |> attempt <|> pModuleDef false) // NOTE: Use a choice here when another type of def is added.
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
