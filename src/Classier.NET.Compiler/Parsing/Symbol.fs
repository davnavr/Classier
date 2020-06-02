namespace Classier.NET.Compiler.Parsing

open FParsec

[<RequireQualifiedAccess>]
type Symbol =
    | Namespace of string list
    | Type of Grammar.TypeHeader

type SymbolOrigin =
    /// Indicates that the symbol originated from some other source, such as a *.dll file.
    | External of string
    | SourceCode of Position
