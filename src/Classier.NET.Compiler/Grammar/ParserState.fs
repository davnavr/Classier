namespace Classier.NET.Compiler.Grammar

open FParsec

type ParserState =
    { Flags: Flags list
      Parents: ResolvedSymbol list
      Symbols: SymbolTable }

module ParserState =
    let defaultState =
        { Flags = List.empty
          Parents = List.empty
          Symbols = SymbolTable.empty }

    let currentFlags state =
        state.Flags
        |> List.tryHead
        |> Option.defaultValue Flags.None

    let currentParent state = List.tryHead state.Parents

    let visibilityFlags state = currentFlags state &&& Flags.VisibilityMask

    let pushFlags flags state: ParserState = { state with Flags = flags :: state.Flags }

    let newFlags state = pushFlags Flags.None state

    let pushParent parent state: ParserState = { state with Parents = parent :: state.Parents }

    let popFlags state =
        match state.Flags with
        | [] -> state
        | _ -> { state with Flags = state.Flags.Tail }

    let setFlags flags state =
        match state.Flags with
        | [] -> state
        | _ ->
            { state with Flags = (currentFlags state ||| flags) :: state.Flags.Tail }

    let popParent state =
        match state.Parents with
        | [] -> state
        | _ -> { state with Parents = state.Parents.Tail }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let clearAllParents state: ParserState = { state with Parents = List.empty }
    let clearAllFlags state: ParserState = { state with Flags = List.empty }

    let enterParentInc =
        let p = (fun () -> invalidOp "Ensure the proper parent is set in this reference cell.") |> ref
        p.Value()
        |> pushParent
        |> updateUserState
        >>. preturn p
