// Copyright (c) 2020, David Navarro. All rights reserved.
// Licensed under the MIT license. For more information, see the 'LICENSE' file in the project root.

module Classier.NET.Compiler.Lexing

open System
open System.IO
open System.Text
open System.Text.RegularExpressions;

type Token<'T> =
    {
        Content: string
        LineNum: int
        LinePos: int
        Type: 'T
    }

/// <summary>
/// Used to read characters from a sequence.
/// </summary>
type CharReader(chars: seq<char>) =
    inherit TextReader()

    do
        if chars = null then
            nullArg "chars"

    let enumerator = chars.GetEnumerator()
    let mutable hasMore = enumerator.MoveNext()
    let mutable curr = int(enumerator.Current)

    override this.Peek() =
        if hasMore then
            int(enumerator.Current)
        else
            -1
    override this.Read() =
        let last = this.Peek()
        hasMore <- enumerator.MoveNext()
        let result = if hasMore then curr else last
        curr <- if hasMore then this.Peek() else -1
        result
    override this.Dispose(disposing) =
        if disposing then
            enumerator.Dispose()

let readLines chars isLine =
    if Seq.isEmpty chars then
        Seq.empty
    else
        seq {
            use reader = new CharReader(chars)
            let mutable line = StringBuilder(1)

            while reader.Peek() >= 0 do
                line.Append(char(reader.Read())) |> ignore

            yield "Test"
        }

let fullLinePattern = Regex("(\\r\\n|\\n|\\r)$")

/// <summary>
/// Determines whether the specified string ends with a newline character.
/// </summary>
/// <param name="line">The <see cref="string"/> that ends with the newline character.</param>
/// <returns>
/// <see langword="true"/> if the string ends with a newline character; otherwise, <see langword="false"/>.
/// </returns>
/// <remarks>
/// For a string to be considered a line, it must end with one of the following:
/// - A carriage return <code>U+000D</code>
/// - A line feed <code>U+000A</code>
/// - A carriage return <code>U+000D</code> followed by a line feed <code>U+000A</code>
/// </remarks>
let isFullLine (line: string) =
    let m = fullLinePattern.Match(line, 0)
    m.Length = line.Length - m.Index

//let tokenize<'T> (src, tmap) =
//    let lines = readLines(src)
