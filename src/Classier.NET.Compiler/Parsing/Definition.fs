namespace Classier.NET.Compiler.Parsing

type Definition =
    { Flags: Flags
      Name: string }

module Definition =
    let ofState name state =
        { Flags = ParserState.currentFlags state
          Name = name }
