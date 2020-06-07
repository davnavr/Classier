namespace Classier.NET.Compiler

open Classier.NET.Compiler.Grammar
open FParsec

type ParserState =
    { Flags: Flags list
      // TODO: We need to keep track of currently parsed members and parent if we want to raise an error when duplicate things are declared.
      // Parents: ParentSymbol list
      // Members: ?
      Symbols: GlobalsTable }

module ParserState =
    let defaultState =
        { Flags = List.empty
          Symbols = GlobalsTable.empty }

    let position: Parser<Position, _> = fun stream -> Reply(stream.Position)

    let currentFlags state =
        state.Flags
        |> List.tryHead
        |> Option.defaultValue Flags.None

    let visibilityFlags state = currentFlags state &&& Flags.VisibilityMask

    let pushFlags flags state: ParserState = { state with Flags = flags :: state.Flags }

    let newFlags state = pushFlags Flags.None state

    let popFlags state =
        match state.Flags with
        | [] -> state
        | _ -> { state with Flags = state.Flags.Tail }

    let setFlags flags state =
        match state.Flags with
        | [] -> state
        | _ ->
            { state with Flags = (currentFlags state ||| flags) :: state.Flags.Tail }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let clearAllFlags state: ParserState = { state with Flags = List.empty }
