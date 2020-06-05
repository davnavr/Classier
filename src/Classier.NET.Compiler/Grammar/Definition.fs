namespace Classier.NET.Compiler.Grammar

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Definition =
    let ofIdentifier state pos identifier =
        { Flags = ParserState.currentFlags state
          Identifier = identifier
          Position = pos }

    let ofState state pos name = ofIdentifier state pos (Identifier.ofString name)
