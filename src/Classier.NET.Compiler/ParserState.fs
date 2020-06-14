namespace Classier.NET.Compiler

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.GlobalType
open FParsec

type ParserState<'Validator> =
    { Members: ImmutableSortedSet<MemberDef> list
      Namespace: FullIdentifier option
      Validator: 'Validator list
      Symbols: GlobalsTable }

module ParserState =
    type ValidationResult<'Validator> =
        | ValidationSuccess of ParserState<'Validator>
        | ValidationError of string

    type Validator = Validator of (ParserState<Validator> -> MemberDef -> ValidationResult<Validator>)
    type ParserState = ParserState<Validator>

    let private errEmptyStack = ValidationError "The member stack was empty"

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

    let newMembers state = { state with Members = emptyMembers :: state.Members }
    let popMembers state =
        match state.Members with
        | [] -> None
        | _ -> Some { state with Members = state.Members.Tail }

    let defaultState =
        { Members = List.empty
          Namespace = None
          Validator = List.empty
          Symbols = GlobalsTable.empty }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    // TODO: Make more detailed error messages for the validators.
    /// Validates the names of non-nested types or modules.
    let typeValidator =
        (fun state mdef ->
            match (mdef, state.Members) with
            | (_, []) -> errEmptyStack
            | (Type tdef, _) ->
                let dupMsg() =
                    MemberDef.name mdef
                    |> sprintf "A type with the name %A already exists in this scope"
                    |> ValidationError
                let typeSymbol =
                    GlobalTypeSymbol.ofTypeDef
                        state.Namespace
                        tdef

                state.Symbols
                |> GlobalsTable.addType typeSymbol
                |> Option.map (fun symbols -> ValidationSuccess { state with Symbols = symbols })
                |> Option.defaultWith dupMsg
            | (_, _) ->
                mdef
                |> string
                |> sprintf "The member %s is not a non-nested type or module"
                |> ValidationError)
        |> Validator

    let memberValidator =
        (fun state mdef ->
            match List.tryHead state.Members with
            | Some members ->
                let dupMsg() =
                    MemberDef.name mdef
                    |> sprintf "A member with the name %A already exists in this scope"
                    |> ValidationError

                members
                |> SortedSet.add(mdef)
                |> Option.map
                    (fun next -> ValidationSuccess { state with Members = next :: state.Members.Tail })
                |> Option.defaultWith dupMsg
            | None -> errEmptyStack)
        |> Validator

    [<AutoOpen>]
    module StateManagement =
        let tryUpdateState f msg =
            getUserState
            >>= fun state ->
                match f state with
                | Some newState -> setUserState newState
                | None -> fail msg
