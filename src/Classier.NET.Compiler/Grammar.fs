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
open Classier.NET.Compiler.SeqParsers
open Classier.NET.Compiler.SyntaxNode

[<Flags>]
type Flags =
    | None = 0
    | Public = 0
    | Internal = 1
    | Protected = 2
    | Private = 3
    | VisibilityMask = 3
    | Inline = 4
    | Abstract = 8
    /// Indicates that a class, field, or local variable is mutable,
    /// or that a method changes the value of a field.
    | Mutable = 16
    /// Indicates that a class can have a subclass.
    | Inheritable = 32

type Definition =
    { Name: string
      Flags: Flags }
    
    member this.Visibility =
        this.Flags ||| Flags.VisibilityMask

type NodeValue =
    | CompilationUnit of
        {| Definitions: seq<SyntaxNode<NodeValue>>
           Imports: seq<seq<string>>
           Namespace: seq<string> |}
    | AccessModifier of Flags
    | ArgumentTuple of seq<SyntaxNode<NodeValue>>
    | ArgumentList of seq<seq<SyntaxNode<NodeValue>>>
    | BinLit
    | Block
    | ClassDef of TypeDef
    | Colon
    | Comma
    | Comment
    | DecLit
    | Expression
    | ExprAdd
    /// A method or function call
    | ExprCall of
        {| Arguments: SyntaxNode<NodeValue>
           Target: SyntaxNode<NodeValue> |}
    | ExprDiv
    | ExprSub
    | ExprMul
    | FieldDef of FieldOrVar
    | FuncDef of MethodOrFunc
    | HexLit
    | Identifier of string
    | IdentifierFull of seq<string>
    | InterfaceDef of TypeDef
    | IntLit
    | Keyword
    | LCurlyBracket
    | LParen
    | MethodDef of MethodOrFunc
    | ModuleDef of TypeDef
    | NamespaceDef of seq<string>
    | Newline
    | OpAdd
    | OpDiv
    | OpEqual
    | OpSub
    | OpMul
    | Param of Param
    | ParamTuple of seq<Param>
    | ParamList of seq<seq<Param>>
    | Period
    | RCurlyBracket
    | RecordDef of
        {| Body: SyntaxNode<NodeValue>
           Definition: Definition
           Fields: seq<FieldOrVar> |}
    | RParen
    | Semicolon
    | StReturn of SyntaxNode<NodeValue>
    | TypeAnnotation of seq<string>
    | UseStatement of seq<string>
    | VariableDef of FieldOrVar
    | Whitespace
and FieldOrVar =
    { Definition: Definition
      ValueType: seq<string>
      Value: SyntaxNode<NodeValue> option }
and MethodOrFunc =
    { Body: SyntaxNode<NodeValue> option
      Definition: Definition
      Parameters: seq<seq<Param>>
      RetValueType: seq<string> }
and Param = { Name: string; Type: seq<string> }
and TypeDef =
    { Body: SyntaxNode<NodeValue>
      Definition: Definition }

let parser: Parser<SyntaxNode<NodeValue>, Flags> =
    let colon = charToken ':' Colon
    let comma = charToken ',' Comma
    let keyword word = strToken word Keyword
    let lcurlybracket = charToken '{' LCurlyBracket
    let lparen = charToken '(' LParen
    let opequal = charToken '=' OpEqual
    let period = charToken '.' Period
    let rcurlybracket = charToken '}' RCurlyBracket
    let rparen = charToken ')' RParen
    let semicolon = charToken ';' Semicolon

    let accessModifier full = // TODO: Make this optional
        let modifiers =
            [
                "public", Flags.Public
                "internal", Flags.Internal
                if full then
                    "protected", Flags.Protected
                    "private", Flags.Private
            ]
        modifiers
        |> Seq.map (fun (str, vis) ->
            pstring str
            |> attempt
            .>>. preturn vis
            .>> setUserState vis)
        |> choice
        |> node (fun (str, vis) -> createToken (AccessModifier vis) str)
        <?> "access modifier"

    let block parser =
        lcurlybracket
        .>>. attempt parser
        .>>. (rcurlybracket <?> "closing bracket")
        |>> (fun ((lc, content), rc) -> seq { lc; yield! content; rc })
        |> node (createNode Block)

    let ignored =
        choice
            [
                anyOf [ ' '; '\t' ]
                <?> "whitespace"
                |> many1Chars
                |> token Whitespace

                newline
                |> node (fun c pos ->
                    { Content = Token (string c)
                      Position = pos.NextLine
                      Value = Newline })

                pstring "//" .>>. restOfLine false
                |>> String.Concat
                |> token Comment
                <?> "single-line comment"
            ]
        |> attempt
        |> many

    let ignored1 = notEmpty ignored

    let modifierChoice words =
        words
        |> Seq.map (fun (word, flag) ->
            keyword word .>> updateUserState (fun f -> f ||| flag))
        |> choice
        .>>. ignored1
        |> opt
        |>> (fun w ->
            match w with
            | Some (wordm, sep) -> seq { wordm; yield! sep }
            | None -> Seq.empty)

    let modifier word flag =
        modifierChoice [ word, flag ]

    let modifierMutable = modifier "mutable" Flags.Mutable

    let identifier =
        asciiLetter
        .>>. many1Chars (asciiLetter <|> digit)
        |>> String.Concat
        |> node (fun name -> createToken (Identifier name) name)
        <??> "identifier"

    let identifierFull =
        many
            (identifier .>>. ignored .>>. period .>>. ignored
            |> attempt
            |>> fun (((name, sep1), per), sep2) ->
                seq {
                    name
                    yield! sep1
                    per
                    yield! sep2
                })
        |>> Seq.collect id
        .>>. identifier
        |>> fun (leading, last) -> seq { yield! leading; last }
        |> node (fun nodes ->
            let names =
                nodes
                |> Seq.choose (fun node ->
                    match node.Value with
                    | Identifier name -> Some name
                    | _ -> None)
            createNode (IdentifierFull names) nodes)

    let typeAnnotation =
        ignored
        .>>. colon
        |> attempt
        .>>. ignored
        .>>. identifierFull
        |>> (fun (((sep1, col), sep2), typeName) ->
            typeName,
            seq {
                yield! sep1
                col
                yield! sep2
                typeName
            })
        |> node (fun (typeName, nodes) ->
            let valueType =
                match typeName.Value with
                | IdentifierFull names -> names
                | _ -> Seq.empty
            createNode (TypeAnnotation valueType) nodes)
        <?> "type annotation"

    let typeAnnotationOpt = opt typeAnnotation

    let nameTypePair =
        identifier .>>. typeAnnotationOpt

    let paramTuple =
        let param =
            nameTypePair
            |> node (fun (name, typeName) ->
                let paramName =
                    match name.Value with
                    | Identifier str -> str
                    | _ -> String.Empty
                let paramType =
                    typeName
                    |> Option.map (fun node ->
                        match node.Value with
                        | TypeAnnotation names -> names
                        | _ -> Seq.empty)
                    |> Option.defaultValue Seq.empty
                let nodes =
                    [
                        name
                        if typeName.IsSome then
                            typeName.Value
                    ]
                createNode (Param { Name = paramName; Type = paramType }) nodes)

        lparen
        .>>. ignored
        .>>. seqOpt
            (many
                (param
                .>>. ignored
                .>>. comma
                .>>. ignored
                |> attempt
                |>> fun (((p, sep1), com), sep2) ->
                    seq {
                        p
                        yield! sep1
                        com
                        yield! sep2
                    })
             |>> Seq.collect id
             .>>. param
             |>> fun (rest, last) -> seq { yield! rest; last})
        .>>. ignored
        .>>. rparen
        |>> (fun ((((lp, sep1), pms), sep2), rp) ->
            pms,
            seq {
                lp
                yield! sep1
                yield! pms
                yield! sep2
                rp
            })
        <?> "parameters"
        |> node (fun (pms, nodes) ->
            let parameters =
                pms
                |> Seq.choose (fun node ->
                    match node.Value with
                    | Param p -> Some p
                    | _ -> None)
            createNode (ParamTuple parameters) nodes)

    let paramList =
        paramTuple
        .>>. (ignored
            .>>. paramTuple
            |> attempt
            |>> fun (sep, tup) -> seq { yield! sep; tup }
            |> many
            |>> Seq.collect id)
        |>> (fun (first, rest) -> seq { first; yield! rest })
        <?> "parameter list"
        |> node (fun nodes ->
            let tuples =
                nodes
                |> Seq.choose (fun node ->
                    match node.Value with
                    | ParamTuple ps -> Some ps
                    | _ -> None)
            createNode (ParamList tuples) nodes)

    let funcKeywords =
        accessModifier true
        .>>. ignored1
        .>>. modifierChoice
                [
                    "abstract", Flags.Abstract
                    "inline", Flags.Inline
                ]
        .>>. modifier "mutator" Flags.Mutable
        .>>. getUserState
        |>> fun ((((acc, sep), worda), wordm), flags) ->
            flags,
            seq {
                acc
                yield! sep
                yield! worda
                yield! wordm
            }

    let expression: Parser<SyntaxNode<NodeValue>, Flags> =
        let numSuffixes suffixes =
            suffixes
            |> List.map pstringCI
            |> choice
            |> opt
            |>> Option.defaultValue String.Empty
            <?> "numeric suffix"
        let decSuffixes = numSuffixes [ "d"; "f"; "m"; ]
        let intSuffixes = numSuffixes [ "l"; "u"; "ul"; "lu" ]
        let intDigits =
            digit
            .>>. manyChars (digit <|> pchar '_')
            |>> fun (leading, trailing) ->
                sprintf "%c%s" leading trailing

        let expr = OperatorPrecedenceParser<_,_,_> ()

        [
            ExprAdd, OpAdd, "+", 1, Associativity.Left;
            ExprSub, OpSub, "-", 1, Associativity.Left;
            ExprMul, OpMul, "*", 5, Associativity.Left;
            ExprDiv, OpDiv, "/", 5, Associativity.Left;
        ]
        |> List.map (fun (exprType, operandType, symbol, prec, assoc) ->
            let mapping op (expr1: SyntaxNode<NodeValue>) (expr2: SyntaxNode<NodeValue>) =
                let opNode = createToken operandType symbol expr1.Position
                let nodes =
                    seq {
                        expr1
                        opNode
                        yield! op
                        expr2
                    }
                createNode exprType nodes expr1.Position
            InfixOperator (symbol, ignored, prec, assoc, (), mapping))
        |> List.iter expr.AddOperator

        let argumentTuple =
            lparen
            .>>. ignored
            .>>. (expr.ExpressionParser
                 .>>. (ignored
                      .>>. comma
                      |> attempt
                      .>>. ignored
                      .>>. expr.ExpressionParser
                      |>> (fun (((sep1, c), sep2), ex) ->
                         seq {
                             yield! sep1
                             c
                             yield! sep2
                             ex
                         })
                      |> many
                      |>> Seq.collect id)
                 .>>. ignored
                 |>> (fun ((first, rest), sep) -> seq { first; yield! rest; yield! sep })
                 |> seqOpt)
            .>>. rparen
            |> attempt
            |>> (fun (((lp, sep1), args), rp) ->
                args
                |> Seq.where (fun node ->
                    match node.Value with
                    | Expression _ -> true
                    | _ -> false),
                seq {
                    lp
                    yield! sep1
                    yield! args
                    rp
                })
            |> node (fun (args, nodes) ->
                createNode (ArgumentTuple args) nodes)

        expr.TermParser <-
            choice
                [
                    identifierFull

                    pstring "-"
                    |> opt
                    |>> Option.defaultValue String.Empty
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

                    lparen
                    .>>. ignored
                    .>>. expr.ExpressionParser
                    .>>. ignored
                    .>>. rparen
                    |>> (fun ((((lp, sep1), exp), sep2), rp) ->
                        seq {
                            lp
                            yield! sep1
                            exp
                            yield! sep2
                            rp
                        })
                    |> node (createNode Expression)
                ]
            .>>. ignored
            .>>. opt
                (argumentTuple
                .>>. (ignored
                     .>>. argumentTuple
                     |>> (fun (sep, arg) -> seq { yield! sep; arg })
                     |> many
                     |>> Seq.collect id)
                |>> (fun (first, rest) -> seq { first; yield! rest })
                |> node (fun nodes ->
                    let argList =
                        nodes
                        |> Seq.choose (fun node ->
                            match node.Value with
                            | ArgumentTuple args -> Some args
                            | _ -> None)
                    createNode (ArgumentList argList) nodes))
            |>> (fun ((ex, sep), args) ->
                match args with
                | Some argsNode ->
                    { Content = Node (seq { ex; yield! sep; argsNode })
                      Position = ex.Position
                      Value = ExprCall {| Arguments = argsNode; Target = ex |} }
                    |> Seq.singleton
                | None -> seq { ex; yield! sep })
            |> node (createNode Expression)

        expr.ExpressionParser <?> "expression"

    let variableDef value =
        keyword "let"
        .>> setUserState Flags.Private
        .>>. ignored1
        .>>. modifierMutable
        .>>. nameTypePair
        .>>. seqOpt
                (ignored
                .>>. opequal
                |> attempt
                .>>. ignored
                .>>. expression
                |>> fun (((sep1, eq), sep2), expr) ->
                    seq {
                        yield! sep1
                        eq
                        yield! sep2
                        expr
                    })
        .>>. ignored
        .>>. semicolon
        |>> (fun ((((((wordl, sep1), wordm), (name, valueType)), value), sep2), smcolon) ->
            valueType,
            name,
            seq {
                wordl
                yield! sep1
                yield! wordm
                name
                if valueType.IsSome then
                    valueType.Value
                yield! value
                yield! sep2
                smcolon
            })
        .>>. getUserState
        |> node (fun ((valueType, name, nodes), flags) ->
            let def =
                { Definition =
                      { Name =
                            match name.Value with
                            | Identifier name -> name
                            | _ -> String.Empty
                        Flags = flags }
                  ValueType =
                      valueType
                      |> Option.map (fun node ->
                          match node.Value with
                          | TypeAnnotation names -> names
                          | _ -> Seq.empty)
                      |> Option.defaultValue Seq.empty
                  Value =
                      nodes
                      |> Seq.tryPick (fun node ->
                          match node.Value with
                          | Expression -> Some node
                          | _ -> None) }
                
            createNode (value def) nodes)

    let fieldDef = variableDef FieldDef <?> "field definition"

    let funcBody =
        ignored
        .>>. choice
            [
                variableDef VariableDef
                <?> "local variable";

                keyword "return"
                .>>. ignored1
                |>> (fun (ret, sep) -> seq { ret; yield! sep })
                |> opt
                .>>. expression
                .>>. semicolon
                |>> (fun ((ret, expr), scolon) ->
                    expr,
                    seq {
                        if ret.IsSome then
                            yield! ret.Value
                        expr
                        scolon
                    })
                |> node (fun (expr, nodes) -> createNode (StReturn expr) nodes)
                <?> "return statement";
            ]
        .>>. ignored
        |> attempt
        |>> (fun ((sep1, statement), sep2) -> seq { yield! sep1; statement; yield! sep2 })
        |> many
        |>> Seq.collect id
        |> block

    let identifierStatement word value =
        keyword word
        .>>. ignored1
        .>>. identifierFull
        .>>. ignored
        .>>. semicolon
        |>> (fun ((((wrd, sep1), id), sep2), sep3) ->
            seq {
                wrd
                yield! sep1
                id
                yield! sep2
                sep3
            })
        |> node (fun nodes ->
            let id =
                nodes
                |> Seq.choose (fun node ->
                    match node.Value with
                    | IdentifierFull names -> Some names
                    | _ -> None)
                |> Seq.collect id
            createNode (value id) nodes)

    let useStatements =
        choice
            [
                identifierStatement "use" UseStatement
                <?> "use statement"
                |>> Seq.singleton;

                ignored1
                |>> Seq.ofList;
            ]
        |> many
        |>> Seq.collect id

    let funcDef value =
        funcKeywords
        .>>. identifier
        .>>. ignored
        .>>. paramList
        |> attempt
        .>>. typeAnnotationOpt
        .>>. ignored
        .>>. choice
            [
                funcBody
                semicolon
            ]
        |>> (fun (((((((flags, modifiers), name), sep1), plist), retVal), sep2), body) ->
            flags, name.ToString(), plist, retVal, body,
            seq {
                yield! modifiers
                name
                yield! sep1
                plist
                if retVal.IsSome then
                    retVal.Value
                yield! sep2
                body
            })
        |> node (fun (flags, name, plist, retVal, body, nodes) ->
            let def =
                { Body =
                    match body.Value with
                    | Block _ -> Some body
                    | _ -> None
                  Definition =
                    { Name = name
                      Flags = flags }
                  Parameters =
                    match plist.Value with
                    | ParamList ps -> ps
                    | _ -> Seq.empty
                  RetValueType =
                    retVal
                    |> Option.map (fun node ->
                        match node.Value with
                        | TypeAnnotation names -> names
                        | _ -> Seq.empty)
                    |> Option.defaultValue Seq.empty }
            createNode (value def) nodes)

    let methodDef = funcDef MethodDef <?> "method definition"

    let memberBlock (parsers: seq<Parser<SyntaxNode<_>, _>>) =
        ignored
        .>>. choice parsers
        .>>. ignored
        |> attempt
        |>> (fun ((sep1, node), sep2) ->
            seq {
                yield! sep1
                node
                yield! sep2
            })
        |> many
        |>> Seq.collect id
        |> block

    let implements =
        ignored1
        .>>. keyword "implements"
        |> attempt
        .>>. ignored1
        .>>. many1
                (identifierFull
                .>>. ignored
                .>>. comma
                .>>. ignored
                |> attempt
                |>> fun (((name, sep1), c), sep2) ->
                    seq {
                        name
                        yield! sep1
                        c
                        yield! sep2
                    })
        |>> (fun (((sep1, wordi), sep2), interfaces) ->
            seq {
                yield! sep1
                wordi
                yield! sep2
                yield! Seq.collect id interfaces
            })
        |> seqOpt

    let classDef nested =
        accessModifier nested
        .>>. ignored1
        .>>. modifierChoice
                [
                    "abstract", Flags.Abstract;
                    "inheritable", Flags.Inheritable;
                ]
        .>>. modifierMutable
        |>> (fun (((acc, sep), worda), wordm) ->
            seq {
                acc
                yield! sep
                yield! worda
                yield! wordm
            })
        .>>. ignored
        .>>. keyword "class"
        |> attempt
        .>>. ignored1
        .>>. identifier
        |>> (fun ((((modifiers, sep1), wordc), sep2), name) ->
                name,
                seq {
                    yield! modifiers
                    yield! sep1
                    wordc
                    yield! sep2
                    name
                })
        .>>. seqOpt
                (ignored1
                .>>. keyword "extends"
                |> attempt
                .>>. ignored1
                .>>. identifierFull
                |>> fun (((sep1, worde), sep2), id) ->
                    seq {
                        yield! sep1
                        worde
                        yield! sep2
                        id
                    })
        .>>. implements
        .>>. ignored
        |>> (fun ((((name, modifiers), extend), iimpl), sep) ->
                name,
                seq {
                    yield! modifiers
                    yield! extend
                    yield! iimpl
                    yield! sep
                })
        .>>. memberBlock
                [
                    fieldDef
                    methodDef
                ]
        |>> (fun ((name, def), body) -> name, body, seq { yield! def; body })
        .>>. getUserState
        |> node (fun ((name, body, nodes), flags) ->
            let def =
                { Definition =
                    { Name = name.ToString()
                      Flags = flags }
                  Body = body }
            createNode (ClassDef def) nodes)
        <?> "class definition"

    let recordDef nested =
        accessModifier nested
        .>>. ignored1
        .>>. keyword "data"
        |> attempt
        .>>. ignored1
        .>>. identifier
        .>>. ignored
        .>>. (ignored
             .>>. identifier
             |> attempt
             .>>. typeAnnotation
             <?> "record field"
             |> node (fun ((sep, valName), valType) ->
                    let nodes = seq { yield! sep; valName; valType }
                    let def =
                        { Definition =
                            { Name = valName.ToString()
                              Flags = Flags.Public }
                          ValueType =
                            match valType.Value with
                            | TypeAnnotation names -> names
                            | _ -> Seq.empty
                          Value = None }
                    createNode (FieldDef def) nodes)
             .>>. ignored
             |>> (fun (field, sep) -> seq { field; yield! sep })
             |> many
             |>> Seq.collect id
             |> block)
        |>> (fun ((((((acc, sep1), word), sep2), name), sep3), body) ->
            name, body,
            seq {
                acc
                yield! sep1
                word
                yield! sep2
                name
                yield! sep3
                body
            })
        .>>. getUserState
        |> node (fun ((name, body, nodes), flags) ->
            let def =
                RecordDef
                    {| Body = body
                       Definition =
                           { Name = name.ToString()
                             Flags = flags }
                       Fields =
                           nodes
                           |> Seq.choose (fun node ->
                               match node.Value with
                               | FieldDef field -> Some field
                               | _ -> None) |}
            createNode def nodes)
        <?> "record definition"

    let moduleDef nested =
        accessModifier nested
        .>>. ignored1
        .>>. keyword "module"
        |> attempt
        .>>. ignored1
        .>>. identifier
        .>>. ignored
        .>>. memberBlock
                [
                    variableDef VariableDef <?> "variable definition";
                    funcDef FuncDef <?> "function definition";
                    recordDef true;
                    classDef true;
                ]
        |>> (fun ((((((acc, sep1), wordm), sep2), name), sep3), body) ->
            name, body,
            seq {
                acc
                yield! sep1
                wordm
                yield! sep2
                name
                yield! sep3
                body
            })
        .>>. getUserState
        |> node (fun ((name, body, nodes), flags) ->
            let def =
                { Definition =
                    { Name = name.ToString()
                      Flags = flags }
                  Body = body }
            createNode (ModuleDef def) nodes)
        <?> "module definition"

    useStatements
    .>>. opt (identifierStatement "namespace" NamespaceDef <?> "namespace definition")
    .>>. useStatements
    .>>. many
            (choice
                ([ classDef; moduleDef; recordDef ]
                 |> List.map (fun f -> f false))
             .>>. ignored
             |>> fun (def, sep) -> seq { def; yield! sep })
    |>> (fun (((use1, ns), use2), defs) ->
        seq {
            yield! use1
            if ns.IsSome then
                ns.Value
            yield! use2
            yield! defs |> Seq.collect id
        })
    .>> eof
    |> node (fun nodes ->
        let cu =
            CompilationUnit
                {| Definitions =
                       nodes
                       |> Seq.where (fun node ->
                           match node.Value with
                           | ClassDef _ | ModuleDef _ -> true
                           | _ -> false)
                   Imports =
                       nodes
                       |> Seq.choose (fun node ->
                           match node.Value with
                           | UseStatement import -> Some import
                           | _ -> None)
                   Namespace =
                       nodes
                       |> Seq.choose (fun node ->
                           match node.Value with
                           | NamespaceDef ns -> Some ns
                           | _ -> None)
                       |> Seq.tryHead 
                       |> Option.defaultValue Seq.empty |}
        createNode cu nodes)
    <?> "compilation unit"
