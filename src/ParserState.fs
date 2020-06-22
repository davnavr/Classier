namespace Classier.NET.Compiler

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.GlobalsTable
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

    type Validator =  Access * MemberDef -> MemberSet -> Result<MemberSet, string>

    type ParserState =
        { EntryPoint: EntryPoint option
          Members: (MemberSet * Validator) list
          Namespace: FullIdentifier option
          Params: ImmutableSortedSet<ParamIdentifier> list
          Symbols: GlobalsTable }

    let pushMembers set validator state: ParserState =
        { state with Members = (set, validator) :: state.Members }
    let popMembers state =
        match state.Members with
        | [] -> Result.Error "Unable to pop the member stack since it is already empty"
        | _ -> Result.Ok { state with Members = state.Members.Tail }
    let replaceMember (acc, mdef) tset setsel state =
        let err =
            mdef.ToString()
            |> sprintf "Unable to replace member %s, %s"
            >> Result.Error
        state.Members
        |> List.tryHead
        |> Option.map
            (fun (mset, v) ->
                match setsel mset with
                | Some validSet ->
                    let def = (acc, mdef)
                    match validSet with
                    | SortedSet.Contains def ->
                        let members =
                            validSet
                            |> SortedSet.remove def
                            |> SortedSet.add def
                            |> tset
                        Result.Ok { state with Members = (members, v) :: state.Members.Tail }
                    | _ -> err "the member was not in the member set"
                | None -> err "The member set was invalid")
        |> Option.defaultValue (err "the member stack is empty")

    let defaultState: ParserState =
        { EntryPoint = None
          Members = List.empty
          Namespace = None
          Params = List.empty
          Symbols = GlobalsTable.empty }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let setNamespace ns state = { state with ParserState.Namespace = ns }

    let newParams state = { state with Params = ImmutableSortedSet.Empty :: state.Params }
    let popParams state =
        match state.Params with
        | [] -> Result.Error "Unable to pop the parameter stack since it was already empty"
        | _ -> Result.Ok { state with Params = state.Params.Tail }

    let getSelfId state =
        match state.Params with
        | [] ->
            Result.Error "Unable to retrieve the self-identifier since the parameter stack is empty"
        | _ ->
            state.Params
            |> Seq.tryPick
                (Seq.tryPick
                    (fun paramid ->
                        match paramid with
                        | ParamIdentifier _ -> None
                        | SelfIdentifier self -> Some self))
            |> Option.map Result.Ok
            |> Option.defaultValue (Result.Error "A self-identifier could not be found")

    [<AutoOpen>]
    module StateManagement =
        let tryBindParser f =
            getUserState
            >>= fun state ->
                match f state with
                | Result.Ok result -> result
                | Result.Error msg -> fail msg
        let tryMapState map = tryBindParser (map >> Result.map preturn)
        let tryUpdateState action = tryBindParser (action >> Result.map setUserState)

        let tryAddMember mdef =
            tryUpdateState
                (fun state ->
                    let strMem = string mdef
                    match List.tryHead state.Members with
                    | Some (members, validate) ->
                        members
                        |> validate mdef
                        |> Result.map
                            (fun added -> { state with Members = (added, validate) :: state.Members.Tail })
                    | None ->
                        strMem
                        |> sprintf "Unable to add member %s, the stack is empty"
                        |> Result.Error)
        let tryAddPlaceholder mdef = tryAddMember (Access.Public, mdef)

        let tryAddParam idtype (param: IdentifierStr) =
            tryUpdateState
                (fun state ->
                    let paramName = string param
                    match List.tryHead state.Params with
                    | Some paramSet ->
                        match SortedSet.tryAdd (idtype param) paramSet with
                        | Some newSet ->
                            Result.Ok { state with Params = newSet :: state.Params.Tail }
                        | None ->
                            paramName
                            |> sprintf "A parameter with the name '%s' already exists"
                            |> Result.Error
                    | None ->
                        paramName
                        |> sprintf "Unable to add parameter %s since the stack was empty"
                        |> Result.Error)
            >>% param

        let tryMembers (selector: MemberSet -> _ option) =
            tryMapState
                (fun state ->
                    state.Members
                    |> List.tryHead
                    |> Option.map (fun (set, _) ->
                        match selector set with
                        | Some result -> Result.Ok result
                        | None -> Result.Error "The member set was of the incorrect type")
                    |> Option.defaultValue (Result.Error "Unable to retrieve members because the member stack is empty"))

        let tryUpdateSymbols msg f =
            tryUpdateState
                (fun state ->
                    match f state.Symbols with
                    | Some newSymbols ->
                        Result.Ok { state with Symbols = newSymbols }
                    | None -> Result.Error msg )
