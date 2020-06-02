namespace Classier.NET.Compiler.Parsing

type Definition =
    { Flags: Flags
      Name: Identifier }

module Definition =
    let ofIdentifier state identifier =
        { Flags = ParserState.currentFlags state
          Name = identifier }
    // TODO: Switch parameters.
    let ofState name state = ofIdentifier state (Identifier.ofString name)
