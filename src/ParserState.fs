namespace Classier.NET.Compiler

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.GlobalType
open FParsec

type ParserState<'Validator> =
    { EntryPoint: EntryPoint option
      Members: ImmutableSortedSet<Access * MemberDef> list
      Namespace: FullIdentifier
      Validators: 'Validator list
      Symbols: GlobalsTable }

module ParserState =
    type Validator = Validator of (ParserState<Validator> -> Access * MemberDef -> Result<ParserState<Validator>, string>)
    type ParserState = ParserState<Validator>

    let private errEmptyStack = sprintf "The %s stack was unexpectedly empty"

    let private emptyMembers =
        { new IComparer<Access * MemberDef> with
            member _.Compare((_, m1), (_, m2)) =
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
    let getMembers state =
        List.tryHead state.Members
        |> Option.defaultValue emptyMembers

    let defaultState: ParserState =
        { EntryPoint = None
          Members = List.empty
          Namespace = FullIdentifier.Empty
          Validators = List.empty
          Symbols = GlobalsTable.empty }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let setNamespace ns state = { state with ParserState.Namespace = ns }

    let pushValidator validator state = { state with Validators = validator :: state.Validators }
    let popValidator state =
        match state.Validators with
        | [] -> None
        | _ -> Some { state with Validators = state.Validators.Tail }
    // TODO: Make more detailed error messages for the validators.
    /// Validates the names of non-nested types or modules.
    let typeValidator = // TODO: This should also make use of the member stack to add the types.
        (fun state (_, mdef) ->
            match mdef with
            | Type tdef ->
                let dupMsg() =
                    MemberDef.name mdef
                    |> sprintf "A type with the name %A already exists in this scope"
                    |> Result.Error
                let typeSymbol =
                    { Namespace = state.Namespace
                      Type = DefinedType tdef }

                state.Symbols
                |> GlobalsTable.addType typeSymbol
                |> Option.map (fun symbols -> Result.Ok { state with Symbols = symbols })
                |> Option.defaultWith dupMsg
            | _ ->
                mdef
                |> string
                |> sprintf "The member %s is not a non-nested type or module"
                |> Result.Error)
        |> Validator
    let memberValidator =
        (fun state (acc, mdef) ->
            match List.tryHead state.Members with
            | Some members ->
                let dupMsg() =
                    MemberDef.name mdef
                    |> sprintf "A member with the name %A already exists in this scope"
                    |> Result.Error

                members
                |> SortedSet.add(acc, mdef)
                |> Option.map
                    (fun next -> Result.Ok { state with Members = next :: state.Members.Tail })
                |> Option.defaultWith dupMsg
            | None ->
                "member"
                |> errEmptyStack
                |> Result.Error)
        |> Validator

    let replacePlaceholder mdef state = state

    [<AutoOpen>]
    module StateManagement =
        let tryUpdateState f msg =
            getUserState
            >>= fun state ->
                match f state with
                | Some newState -> setUserState newState
                | None -> fail msg

        let tryPopMembers: Parser<_, ParserState> =
            tryUpdateState
                popMembers
                (errEmptyStack "member")

        let tryPopValidators: Parser<_, ParserState> =
            tryUpdateState
                popValidator
                (errEmptyStack "validator")

        let tryAddMember mdef =
            getUserState
            >>= fun state ->
                match List.tryHead state.Validators with
                | Some (Validator validator) ->
                    let result = validator state mdef
                    match result with
                    | Result.Ok newState -> setUserState newState
                    | Result.Error msg -> fail msg
                | None ->
                    string mdef
                    |> sprintf "The validator stack was empty while trying to add the member %s"
                    |> fail
