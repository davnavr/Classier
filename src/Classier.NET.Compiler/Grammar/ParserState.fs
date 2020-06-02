namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

type ParserState =
    { Flags: ImmutableStack<Flags>
      Parents: ImmutableStack<ResolvedSymbol>
      Symbols: SymbolTable }

module ParserState =
    let defaultState =
        { Flags = ImmutableStack.Empty
          Parents = ImmutableStack.Empty
          Symbols = SymbolTable.empty }

    let currentFlags state =
        if state.Flags.IsEmpty
        then Flags.None
        else state.Flags.Peek()

    let currentParent state =
        if state.Parents.IsEmpty
        then None
        else Some (state.Parents.Peek())

    let visibilityFlags state = currentFlags state &&& Flags.VisibilityMask

    let pushFlags flags state: ParserState = { state with Flags = state.Flags.Push(flags) }

    let newFlags state = pushFlags Flags.None state

    let pushParent parent state = { state with Parents = state.Parents.Push(parent) }

    let popFlags state =
        if state.Flags.IsEmpty
        then state
        else { state with Flags = state.Flags.Pop() }

    let setFlags flags state =
        let newFlags = currentFlags state ||| flags
        state
        |> popFlags
        |> pushFlags newFlags

    let popParent state =
        if state.Parents.IsEmpty
        then state
        else { state with Parents = state.Parents.Pop() }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let clearAllParents state: ParserState = { state with Parents = ImmutableStack.Empty }
    let clearAllFlags state: ParserState = { state with Flags = ImmutableStack.Empty }
