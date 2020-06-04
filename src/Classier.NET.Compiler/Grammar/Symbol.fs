namespace Classier.NET.Compiler.Grammar

open FParsec

[<RequireQualifiedAccess>]
type Symbol =
    | Incomplete of Lazy<Symbol>
    | Namespace of string list
    | Type of TypeDef

type SymbolOrigin =
    /// Indicates that the symbol originated from some other source, such as a *.dll file.
    | External of string
    | SourceCode of Position
