namespace Classier.NET.Compiler

open FParsec

type CompilerError =
    | ParserError of ParserError

module CompilerError =
    let ofParserResult parserResult =
        match parserResult with
        | Success _ -> None
        | Failure (_, err, _) -> ParserError err |> Some
