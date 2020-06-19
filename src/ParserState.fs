namespace Classier.NET.Compiler

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.GlobalsTable
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Identifier
open FParsec

type ParserState<'Validator> =
    { EntryPoint: EntryPoint option
      Members: ImmutableSortedSet<Access * MemberDef> list
      Namespace: FullIdentifier option
      Validators: 'Validator list
      SelfIdentifiers: IdentifierStr option list
      Symbols: GlobalsTable }

module ParserState =
    type Validator = Validator of (ParserState<Validator> -> Access * MemberDef -> Result<ParserState<Validator>, string>)
    type ParserState = ParserState<Validator>

    let private errEmptyStack = sprintf "The %s stack was unexpectedly empty"

    let newMembers state = { state with Members = MemberDef.emptyMemberSet :: state.Members }
    let popMembers state =
        match state.Members with
        | [] -> None
        | _ -> Some { state with Members = state.Members.Tail }
    let getMembers state =
        state.Members
        |> List.tryHead 
        |> Option.defaultValue MemberDef.emptyMemberSet
    let private addMember (acc, mdef) dup state =
        match List.tryHead state.Members with
        | Some members ->
            members
            |> SortedSet.add(acc, mdef)
            |> Option.map
                (fun next -> Result.Ok { state with Members = next :: state.Members.Tail })
            |> Option.defaultWith dup
        | None ->
            "member"
            |> errEmptyStack
            |> Result.Error

    let defaultState: ParserState =
        { EntryPoint = None
          Members = List.empty
          Namespace = None
          Validators = List.empty
          SelfIdentifiers = List.empty
          Symbols = GlobalsTable.empty }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let setNamespace ns state = { state with ParserState.Namespace = ns }

    let pushValidator validator state = { state with Validators = validator :: state.Validators }
    let popValidator state =
        match state.Validators with
        | [] -> None
        | _ -> Some { state with Validators = state.Validators.Tail }
    let typeValidator =
        (fun state (acc, mdef) ->
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
                |> Option.map
                    (fun symbols ->
                        { state with Symbols = symbols }
                        |> addMember
                            (acc, mdef)
                            dupMsg)
                |> Option.defaultWith dupMsg
            | _ ->
                mdef
                |> string
                |> sprintf "The member %s is not a non-nested type or module"
                |> Result.Error)
        |> Validator
    let memberValidator =
        (fun state (acc, mdef) ->
            let dupMsg() =
                MemberDef.name mdef
                |> sprintf "A member with the name %A already exists in this scope"
                |> Result.Error
            state
            |> addMember
                (acc, mdef)
                dupMsg)
        |> Validator

    let replacePlaceholder mdef state = state // NOTE: Not implemented yet.

    let pushSelfId selfid state = { state with SelfIdentifiers = selfid :: state.SelfIdentifiers }

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

        let tryAddMember mdef = // TODO: preturn the mdef.
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

        let memberSection p =
            (newMembers >> pushValidator memberValidator)
            |> updateUserState
            >>. p
            .>> tryPopMembers
            .>> tryPopValidators

        // TODO: Create common functions for matching stacks.
        let trySelfId: Parser<_, ParserState> =
            getUserState
            >>= fun state ->
                state.SelfIdentifiers
                |> List.tryHead
                |> Option.flatten
                |> Option.map preturn
                |> Option.defaultValue ("self-identifier" |> errEmptyStack |> fail)
        let tryPushSelfId selfid =
            Some selfid
            |> pushSelfId
            |> updateUserState
            >>% selfid
        let tryPopSelfId: Parser<_, ParserState> =
            getUserState
            >>= fun state ->
                    match state.SelfIdentifiers with
                    | [] -> "self-identifier" |> errEmptyStack |> fail
                    | _ -> setUserState { state with SelfIdentifiers = state.SelfIdentifiers.Tail }
        let tryReplaceSelfId selfid =
            getUserState
            >>= fun state ->
                state.SelfIdentifiers
                |> List.tryHead
                |> Option.map
                    (fun current ->
                        match current with
                        | Some existing ->
                            string existing
                            |> sprintf "Cannot replace existing self-identifier '%s'"
                            |> fail
                        | None ->
                            setUserState { state with SelfIdentifiers = Some selfid :: state.SelfIdentifiers.Tail })
                |> Option.defaultValue ("self-identifier" |> errEmptyStack |> fail)
