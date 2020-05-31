namespace Classier.NET.Compiler

type Definition =
    { Flags: Flags
      Name: string }

module Definition =
    let ofState name state =
        { Flags = ParserState.currentFlags state
          Name = name }
