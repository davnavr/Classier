namespace Classier.NET.Compiler

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.GlobalsTable
open Classier.NET.Compiler.GlobalType
open Classier.NET.Compiler.Identifier
open FParsec

module ParserState =
    [<CustomComparison>]
    [<CustomEquality>]
    type ParamIdentifier =
        | ParamIdentifier of IdentifierStr
        | SelfIdentifier of IdentifierStr

        member private this.Identifier =
            match this with
            | ParamIdentifier id
            | SelfIdentifier id -> id

        override this.Equals obj =
            match obj with
            | :? ParamIdentifier as other ->
                this.Identifier = other.Identifier
            | _ -> false

        override this.GetHashCode() = this.Identifier.GetHashCode()

        interface System.IComparable with
            member this.CompareTo obj =
                let other = obj :?> ParamIdentifier
                compare this.Identifier other.Identifier

    type ParserState =
        { EntryPoint: EntryPoint option
          Members: ImmutableSortedSet<Access * MemberDef> list
          Namespace: FullIdentifier option
          Params: ImmutableSortedSet<ParamIdentifier> list
          Validators: Validator list
          Symbols: GlobalsTable }
    and Validator = Validator of (ParserState -> Access * MemberDef -> Result<ParserState, string>)

    let private errEmptyStack = sprintf "The %s stack was unexpectedly empty"

    let private popStack stack action state =
        match stack state with
        | [] -> None
        | items -> action items |> Some

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
            |> SortedSet.tryAdd(acc, mdef)
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
          Params = List.empty
          Validators = List.empty
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

    let replacePlaceholder mdef state =
        let members = getMembers state
        match members with
        | SortedSet.Contains mdef ->
            let newMembers =
                members
                |> SortedSet.remove mdef
                |> SortedSet.add mdef
            { state with Members = newMembers :: state.Members.Tail }
        | _ -> state

    let newParams state = { state with Params = ImmutableSortedSet.Empty :: state.Params }

    let getSelfId state =
        match state.Params with
        | [] -> None
        | _ ->
            state.Params
            |> Seq.tryPick
                (Seq.tryPick
                    (fun paramid ->
                        match paramid with
                        | ParamIdentifier _ -> None
                        | SelfIdentifier self -> Some self))

    [<AutoOpen>]
    module StateManagement =
        let tryUpdateState f msg =
            getUserState
            >>= fun state ->
                match f state with
                | Some newState -> setUserState newState
                | None -> fail msg

        // TODO: Make the functions below regular functions in the ParserState module that do not return Parser<_,_>. Use tryUpdateState to call these new functions
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

        let tryPushParam idtype (param: IdentifierStr) =
            getUserState
            >>= fun state ->
                match List.tryHead state.Params with
                | Some paramSet ->
                    match SortedSet.tryAdd (idtype param) paramSet with
                    | Some newSet ->
                        { state with Params = newSet :: state.Params.Tail }
                        |> setUserState
                        >>% param
                    | None ->
                        param
                        |> string
                        |> sprintf "A parameter with the name '%s' already exists"
                        |> fail
                | None -> "parameter" |> errEmptyStack |> fail
        let tryPopParams =
            getUserState
            >>= fun state ->
                match state.Params with
                | [] -> "parameters" |> errEmptyStack |> fail
                | _ -> setUserState { state with Params = state.Params.Tail }

        let trySelfId =
            getUserState
            >>= fun state ->
                match state.Params with
                | [] -> "parameters" |> errEmptyStack |> fail
                | _ ->
                    state
                    |> getSelfId
                    |> Option.map preturn
                    |> Option.defaultValue (fail "A self-identifier could not be found.")
