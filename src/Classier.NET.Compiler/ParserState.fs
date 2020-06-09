namespace Classier.NET.Compiler

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Grammar

type ParserState =
    { Flags: Flags list
      Members: ImmutableSortedSet<MemberDef> list
      Symbols: GlobalsTable }

module ParserState =
    let private emptyMembers =
        { new IComparer<MemberDef> with
              member _.Compare(m1, m2) =
                  let paramCompare =
                      compare
                          (MemberDef.firstParams m1)
                          (MemberDef.firstParams m2)
                  match paramCompare with
                  | 0 ->
                      match (m1, m2) with
                      | (Function _, Method _) -> -1
                      | (Method _, Function _) -> 1
                      | (_, _) ->
                          compare
                              (MemberDef.identifier m1)
                              (MemberDef.identifier m2)
                  | _ -> paramCompare }
        |> ImmutableSortedSet.Empty.WithComparer

    let defaultState =
        { Flags = List.empty
          Members = List.empty
          Symbols = GlobalsTable.empty }

    let currentFlags state =
        state.Flags
        |> List.tryHead
        |> Option.defaultValue Flags.None

    let currentMembers state = state.Members.Head

    let visibilityFlags state = currentFlags state &&& Flags.VisibilityMask

    let pushFlags flags state: ParserState = { state with Flags = flags :: state.Flags }
    let newFlags = pushFlags Flags.None

    let popFlags state =
        match state.Flags with
        | [] -> state
        | _ -> { state with Flags = state.Flags.Tail }

    let setFlags flags state =
        match state.Flags with
        | [] -> state
        | _ ->
            { state with Flags = (currentFlags state ||| flags) :: state.Flags.Tail }

    let newMembers state: ParserState = { state with Members = emptyMembers :: state.Members }
    
    let popMembers state =
        match state.Members with
        | [] -> state
        | _ -> { state with Members = state.Members.Tail }

    let addMember mdef state =
        match state.Members with
        | [] -> state
        | _ ->
            { state with Members = state.Members.Head.Add(mdef) :: state.Members.Tail }

    let updateSymbols f state = { state with Symbols = f state.Symbols }
    let clearAllFlags state: ParserState = { state with Flags = List.empty }
