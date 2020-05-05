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
    | BinLit
    | Block
    | ClassDef // TODO: Add anonymous records to some to store information.
    | Colon
    | Comma
    | Comment
    | Expression
    | ExprAdd
    | FieldDef
    | HexLit
    | Identifier
    | IdentifierChain
    | IntLit
    | Keyword
    | LCurlyBracket
    | LParen
    | MethodDef
    | MethodHeader
    | ModuleDef
    | NamespaceDef
    | Newline
    | OpAdd
    | OpEqual
    | Param // of {| Name: string |}
    | ParamTuple // of seq<SyntaxNode<NodeValue>>
    | ParamSet
    | Period
    | RCurlyBracket
    | RParen
    | TypeAnnotation
    | UseStatement
    | Whitespace

let parser: Parser<SyntaxNode<NodeValue>, unit> =
    let pIgnored multiline =
        choice
            [
                anyOf [ ' '; '\t' ]
                <?> "whitespace"
                |> many1Chars
                |> pnode (SyntaxNode.createToken Whitespace);

                // pMlCommentOneLine

                if multiline then
                    unicodeNewline
                    |> pnode (fun c pos ->
                        { Content = Token (string c)
                          Position = pos.NextLine
                          Value = Newline })
                    <?> "newline";

                    pstring "//" .>>. restOfLine false
                    |> pnode (fun (str1, str2) ->
                        SyntaxNode.createToken Comment (str1 + str2));

                    // pMlComment
            ]

    let pIgnoredOpt multiline = opt (pIgnored multiline)

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
        <??> "Identifier"

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
        <?> "Fully qualified name"

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
        .>>. attempt p
        .>>. pcharToken '}' RCurlyBracket
        |>> fun (((leading, lc), middle), rc) ->
            leading @ (lc :: middle @ [ rc ])
        |> pnode (createNode Block)

    let pExpression: Parser<SyntaxNode<NodeValue>, unit> =
        let numSuffixes =
            [ "l"; "u"; "ul"; "lu" ]
            |> List.map pstringCI
            |> choice
            |> opt
            |>> Option.defaultValue ""
            <?> "numeric literal suffix"

        let expr, exprRef = createParserForwardedToRef<SyntaxNode<NodeValue>, unit>()

        let binaryOperator op value = // TODO: Fix, throws a StackOverflowException when parsing addition expression. Need to check something first before parsing expr
            expr
            .>>. pIgnoredOpt true
            .>>. (pstring op |> pnode (createToken value))
            .>>. pIgnoredOpt true
            .>>. expr
            |>> fun ((((exp1, sep1), opstr), sep2), exp2) ->
                [
                    exp1
                    if sep1.IsSome then
                        sep1.Value
                    opstr
                    if sep2.IsSome then
                        sep2.Value
                    exp2
                ]

        exprRef :=
            choice
                [
                    pIdentifier

                    digit // TODO: Allow negative
                    .>>. manyChars (digit <|> pchar '_')
                    .>>. numSuffixes
                    |>> (fun ((first, rest), suffix) ->
                        sprintf "%c%s%s" first rest suffix)
                    |> pnode (createToken IntLit)
                    <??> "integer literal";

                    pstringCI "0x"
                    .>>. hex
                    .>>. manyChars (hex <|> pchar '_')
                    .>>. numSuffixes
                    |>> (fun (((prefix, first), rest), suffix) ->
                        sprintf "%s%c%s%s" prefix first rest suffix)
                    |> pnode (createToken HexLit)
                    <??> "hexadecimal literal";

                    pstringCI "0b"
                    .>>. anyOf [ '0'; '1' ]
                    .>>. manyChars (anyOf [ '0'; '1'; '_' ])
                    .>>. numSuffixes
                    |>> (fun (((prefix, first), rest), suffix) ->
                        sprintf "%s%c%s%s" prefix first rest suffix)
                    |> pnode (createToken BinLit)
                    <??> "binary literal";

                    pcharToken '(' LParen
                    .>>. pIgnoredOpt true
                    .>>. expr // TODO: Make expr optional to allow something like F# unit?
                    .>>. pIgnoredOpt true
                    .>>. pcharToken ')' RParen
                    |>> (fun ((((lparen, sep1), exp), sep2), rparen) ->
                        [
                            lparen
                            if sep1.IsSome then
                                sep1.Value
                            exp
                            if sep2.IsSome then
                                sep2.Value
                            rparen
                        ])
                    |> pnode (createNode Expression);

                    //binaryOperator "+" OpAdd
                    //|> pnode (createNode ExprAdd);
                ]
            <??> "expression"

        expr

    let pTypeAnnotation =
        pIgnored true
        |> attempt
        |> many
        .>>. pcharToken ':' Colon
        .>>. pIgnoredOpt false
        .>>. pIdentifierChain
        |>> (fun (((sep1, col), sep2), typeName) ->
            let trailing =
                [
                    col
                    if sep2.IsSome then
                        sep2.Value
                    typeName
                ]
            sep1 @ trailing)
        |> attempt
        |> pnodesOpt
        |> pnode (createNode TypeAnnotation)
        <?> "type annotation"

    let pNameAndType =
        pIdentifier .>>. pTypeAnnotation |> pnodePair

    let pFieldOrVar =
        pKeyword "let"
        // TODO: Add modifiers "mutable"
        .>>. pNameAndType
        |>> (fun (wordl, names) -> wordl @ names)
        .>>. pIgnoredOpt false
        .>>. pcharToken '=' OpEqual
        .>>. pIgnoredOpt false
        .>>. pExpression
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
        .>>. pIgnoredOpt false
        .>>. pnodesOpt (
            many (
                pParam
                .>>. pIgnoredOpt false
                .>>. pcharToken ',' Comma
                .>>. pIgnoredOpt false
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
        .>>. pIgnoredOpt false
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
        <?> "parameter tuple"

    let pFuncParamList = // TODO: Allow omitting of parenthesis for one parameter in tuple, like F#.
        pFuncParamTuple
        .>>. many (
            pIgnored true
            |> attempt
            |> many
            .>>. pFuncParamTuple
            |> attempt
            |>> fun (sep, tup) -> sep @ [ tup ])
        |>> (fun (first, rest) ->
            first :: List.collect id rest)
        |> pnode (createNode ParamSet)
        <??> "parameter list"

    let pFuncBody: Parser<SyntaxNode<NodeValue>, unit> =
        choice
            [
                pIgnored true
                pExpression
            ]
        |> attempt
        |> many
        |> pBlock
        <??> "function body"

    let pMemberBlock instance =
        choice
            [
                pIgnored true

                pFieldOrVar <?>
                    (if instance
                    then "field definition"
                    else "value definition");

                if instance then
                    pFuncKeywords
                    .>>. (pIdentifier <?> "self identifier")
                    .>>. pIgnoredOpt false
                    .>>. pcharToken '.' Period
                    .>>. pIgnoredOpt false
                    .>>. (pIdentifier <?> "method name")
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
                    .>>. pTypeAnnotation
                    .>>. pFuncBody
                    |>> (fun (((header, mparams), returnType), body) -> [ header; mparams; returnType; body ])
                    |> pnode (createNode MethodDef)
                    <?> "method definition";
            ]
        |> attempt
        |> many
        |> pBlock

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
        |> attempt // Important, allows a module to be parsed if a class couldn't be parsed.
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
    .>>. (pClassDef false <|> pModuleDef false) // NOTE: Use a choice here when another type of def is added.
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
