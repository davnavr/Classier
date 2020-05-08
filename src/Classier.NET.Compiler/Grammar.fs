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

open System

open FParsec

open Classier.NET.Compiler.NodeParsers
open Classier.NET.Compiler.SyntaxNode

// TODO: Switch to using Seq.append instead of List as it is much faster for larger collections.

type Visibility =
    | Public
    | Internal
    | Protected
    | Private

type MemberDef<'Flags> =
    { Name: string
      Visibility: Visibility
      Flags: 'Flags }

[<Flags>]
type ClassFlags =
    | None = 0
    | Abstract = 1
    | Inheritable = 2
    | Mutable = 4

[<Flags>]
type MethodFlags =
    | None = 0
    | Abstract = 1
    | Inline = 2
    | Mutator = 4

[<Flags>]
type FuncFlags =
    | None = 0
    | Inline = 1

type NodeValue =
    | CompilationUnit of
        {| Definition: SyntaxNode<NodeValue>
           Imports: seq<seq<string>>
           Namespace: seq<string> option |}
    | AccessModifier of Visibility
    | BinLit
    | Block
    | ClassDef of MemberDef<ClassFlags>
    | Colon
    | Comma
    | Comment
    | DecLit
    | Expression
    | ExprAdd
    | ExprDiv
    | ExprSub
    | ExprMul
    | FieldDef of MemberDef<unit>
    | FuncDef of MemberDef<unit>
    | HexLit
    | Identifier of string
    | IdentifierChain of seq<string>
    | IntLit
    | Keyword
    | LCurlyBracket
    | LParen
    | MethodDef of MemberDef<MethodFlags> * seq<seq<Param>>
    | MethodHeader
    | ModuleDef of MemberDef<unit>
    | NamespaceDef of seq<string>
    | Newline
    | OpAdd
    | OpDiv
    | OpEqual
    | OpSub
    | OpMul
    | Param of Param
    | ParamTuple of seq<Param>
    | ParamSet of seq<seq<Param>>
    | Period
    | RCurlyBracket
    | RParen
    | Semicolon
    | TypeAnnotation of seq<string>
    | UseStatement
    | Whitespace
and Param = { Name: string; ParamType: seq<string> }

let parser: Parser<SyntaxNode<NodeValue>, unit> = // TODO: Add semicolons to end statements;
    let optString p = opt p |>> Option.defaultValue ""

    let lparen = charToken '(' LParen
    let period = charToken '.' Period
    let rparen = charToken ')' RParen
    let semicolon = charToken ';' Semicolon

    let pword word value = // TODO: Use pstrToken instead
        pstring word |> node (createToken value)

    let pIgnored multiline = // TODO: Make it parse many1?
        let pMLCommentStart = pword "/*" Comment <?> "start of multi-line comment"
        let pMLCommentEnd = pword "*/" Comment <?> "end of multi-line comment"
        let pCommentContent =
            // TODO: Fix, */ might be skipped over, don't know why it parses successfully without changing state
            manyCharsTill anyChar (followedBy (pMLCommentEnd |>> ignore) <|> followedBy (pchar '\n' |>> ignore)) // TODO: Maybe use restOfLine?
            |> node (createToken Comment)
        let pNewline =
            newline
            |> node (fun c pos ->
                { Content = Token (string c)
                  Position = pos.NextLine
                  Value = Newline })
            <?> "newline";

        choice
            [
                anyOf [ ' '; '\t' ]
                <?> "whitespace"
                |> many1Chars
                |> node (SyntaxNode.createToken Whitespace);

                if multiline then
                    pNewline;

                    pstring "//" .>>. restOfLine false
                    |> node (fun (str1, str2) ->
                        SyntaxNode.createToken Comment (str1 + str2))
                    <?> "comment";

                    pMLCommentStart
                    .>>. many (pCommentContent <|> pNewline)
                    .>>. pMLCommentEnd
                    |>> (fun ((start, content), cend) ->
                        start :: content @ [ cend ])
                    |> node (createNode Comment)
                    |> attempt
                else
                    pMLCommentStart
                    .>>. opt pCommentContent
                    .>>. pMLCommentEnd
                    |>> (fun ((cstart, content), cend) ->
                        [
                            cstart
                            if content.IsSome then
                                content.Value
                            cend
                        ])
                    |> node (createNode Comment);
            ]

    let pIgnoredOpt multiline = opt (pIgnored multiline)

    let pKeywordNS keyword = pword keyword Keyword

    let pKeyword keyword =
        pKeywordNS keyword
        .>>. pIgnored false
        |> nodePair

    let pKeywordOpt keyword = nodesOpt (pKeyword keyword)

    let pIdentifier =
        let palphabet =
            List.append [ 'a'..'z' ] [ 'A'..'Z' ]
            |> anyOf
        palphabet
        .>>. many (palphabet <|> digit)
        |>> fun (c, rest) ->
            c :: rest
            |> Array.ofList
            |> String
        |> node (SyntaxNode.createToken Identifier)
        <?> "identifier"

    let pIdentifierChain =
        many
            (pIdentifier
            .>>. pIgnoredOpt false
            .>>. period
            .>>. pIgnoredOpt false
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
        |> node (SyntaxNode.createNode IdentifierChain)
        <?> "fully qualified name"

    let pIdentifierStatement keyword value =
        pKeyword keyword
        .>>. pIdentifierChain
        |>> fun (word, identifier) -> List.append word [identifier]
        |> node (SyntaxNode.createNode value)

    let useStatements =
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
        |> node (SyntaxNode.createToken AccessModifier)
        .>>. pIgnored false
        |> nodePair
        |> nodesOpt
        <?> "access modifier"

    let pBlock (p: NodeListParser<NodeValue>) =
        many (pIgnored true)
        .>>. charToken '{' LCurlyBracket
        .>>. attempt p
        .>>. charToken '}' RCurlyBracket
        |>> fun (((leading, lc), middle), rc) ->
            leading @ (lc :: middle @ [ rc ])
        |> node (createNode Block)

    let pTypeAnnotation =
        pIgnored true
        |> attempt
        |> many
        .>>. charToken ':' Colon
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
        |> nodesOpt
        |> node (createNode TypeAnnotation)
        <?> "type annotation"

    let pNameAndType =
        pIdentifier .>>. pTypeAnnotation |> nodePair

    let pFuncBody, pFuncBodyRef = createParserForwardedToRef<SyntaxNode<NodeValue>, unit>()

    let pExpression: Parser<SyntaxNode<NodeValue>, unit> =
        let numSuffixes suffixes =
            suffixes
            |> List.map pstringCI
            |> choice
            |> optString
            <?> "numeric suffix"
        let decSuffixes = numSuffixes [ "d"; "f"; "m"; ]
        let intSuffixes = numSuffixes [ "l"; "u"; "ul"; "lu" ]
        let intDigits =
            digit
            .>>. manyChars (digit <|> pchar '_')
            |>> fun (leading, trailing) ->
                sprintf "%c%s" leading trailing

        let expr = OperatorPrecedenceParser<_, _, _>()

        [
            ExprAdd, OpAdd, "+", 1, Associativity.Left;
            ExprSub, OpSub, "-", 1, Associativity.Left;
            ExprMul, OpMul, "*", 5, Associativity.Left;
            ExprDiv, OpDiv, "/", 5, Associativity.Left;
        ]
        |> List.map (fun (exprType, operandType, symbol, prec, assoc) ->
            let mapping op (expr1: SyntaxNode<NodeValue>) expr2 =
                let opNode = createToken operandType symbol expr1.Position
                createNode exprType (expr1 :: opNode :: op @ [ expr2 ]) expr1.Position
            InfixOperator (symbol, pIgnored true |> many, prec, assoc, (), mapping))
        |> List.iter expr.AddOperator

        expr.TermParser <-
            choice
                [
                    pIdentifier

                    pstring "-"
                    |> optString
                    .>>. choice
                        [
                            pstringCI "0x"
                            .>>. hex
                            .>>. manyChars (hex <|> pchar '_')
                            .>>. intSuffixes
                            |>> (fun (((prefix, first), rest), suffix) ->
                                sprintf "%s%c%s%s" prefix first rest suffix)
                            .>>. preturn HexLit
                            <?> "hexadecimal literal";

                            pstringCI "0b"
                            .>>. anyOf [ '0'; '1' ]
                            .>>. manyChars (anyOf [ '0'; '1'; '_' ])
                            .>>. intSuffixes
                            |>> (fun (((prefix, first), rest), suffix) ->
                                sprintf "%s%c%s%s" prefix first rest suffix)
                            .>>. preturn BinLit
                            <?> "binary literal";

                            intDigits
                            .>>. pstring "."
                            .>>. intDigits
                            .>>. decSuffixes
                            |>> (fun (((intp, per), decp), suffix) -> intp + per + decp + suffix)
                            .>>. preturn DecLit
                            |> attempt
                            <?> "numeric literal"

                            intDigits
                            .>>. intSuffixes
                            |>> (fun (digits, suffix) -> digits + suffix)
                            .>>. preturn IntLit
                            <?> "integer literal";
                        ]
                    |>> (fun (sign, (node, numType)) -> sign + node, numType)
                    |> node (fun (content, numType) -> createToken numType content);

                    //pIdentifierChain
                    //.>>. pIgnoredOpt false // Method or function call

                    lparen
                    .>>. pIgnoredOpt true
                    .>>. expr.ExpressionParser
                    .>>. pIgnoredOpt true
                    .>>. rparen
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
                    |> node (createNode Expression);
                ]
            .>>. (pIgnored true |> many)
            |> node (fun (e, ignored) pos ->
                if List.isEmpty ignored
                then e
                else createNode Expression (e :: ignored) pos)

        expr.ExpressionParser <?> "expression"

    let pFieldOrVar =
        pKeyword "let"
        // TODO: Add modifiers "mutable"
        .>>. pNameAndType
        |>> (fun (wordl, names) -> wordl @ names)
        .>>. pIgnoredOpt false
        .>>. charToken '=' OpEqual
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
        |> node (createNode FieldDef) // TODO: Differentiate between fields and local variables

    let pFuncKeywords =
        pAccessModifier true
        // TODO: Add modifiers "mutator", "abstract", and "inline"

    let pFuncParamTuple =
        let pParam =
            pNameAndType
            |> node (createNode Param)

        lparen
        .>>. pIgnoredOpt false
        .>>. nodesOpt (
            many (
                pParam
                .>>. pIgnoredOpt false
                .>>. charToken ',' Comma
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
        .>>. rparen
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
        |> node (createNode ParamTuple)
        <?> "parameter tuple"

    let pFuncParamList =
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
        |> node (createNode ParamSet)
        <?> "parameter list"

    pFuncBodyRef :=
        choice
            [
                pIgnored true
                pExpression
            ]
        |> attempt
        |> many
        |> pBlock
        <?> "function body"

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
                    .>>. period
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
                    |> node (createNode MethodHeader)
                    .>>. pFuncParamList
                    .>>. pTypeAnnotation
                    .>>. pFuncBody
                    |>> (fun (((header, mparams), returnType), body) -> [ header; mparams; returnType; body ])
                    |> node (createNode MethodDef)
                    <?> "method definition";
            ]
        |> attempt
        |> many
        |> pBlock

    let pClassDef nested =
        pAccessModifier nested
        .>>. nodesOpt (
            nodesOpt (
                (pKeyword "inheritable" |> attempt <|> pKeyword "abstract")
                .>>. pIgnored false
                |>> fun (impl, sep) -> impl @ [ sep ] )
            .>>. nodesOpt (
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
        |> node (SyntaxNode.createNode ClassDef)
        <?> "class definition"

    let pModuleDef nested =
        pAccessModifier nested
        .>>. pKeyword "module"
        .>>. pIdentifier
        |>> (fun ((access, wordm), name) ->
            List.collect id [ access; wordm; [ name ] ])
        .>>. pMemberBlock false
        |>> fun (header, body) -> header @ [ body ]
        |> node (SyntaxNode.createNode ModuleDef)
        <?> "module definition"

    useStatements
    .>>. opt (pIdentifierStatement "namespace" NamespaceDef <?> "namespace definition")
    .>>. useStatements
    
