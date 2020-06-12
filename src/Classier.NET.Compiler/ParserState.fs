namespace Classier.NET.Compiler

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.GlobalType

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

    let addMember mdef state =
        let members = state.Members
        match members with
        | [] -> None
        | _ ->
            Some { state with Members = members.Head.Add(mdef) :: members.Tail }

    let defaultState =
        { Members = List.empty
          Namespace = None
          Validator = List.empty
          Symbols = GlobalsTable.empty }

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    /// Validates the names of non-nested types or modules.
    let typeValidator =
        (fun state mdef ->
            match mdef with
            | Type tdef ->
                let otherTypes =
                    GlobalsTable.getTypes
                        state.Namespace
                        state.Symbols
                let typeSymbol =
                    GlobalTypeSymbol.ofTypeDef
                        state.Namespace
                        tdef

                if otherTypes.Contains typeSymbol then
                    ValidationError "test"
                else
                    match addMember mdef state with
                    | Some newState ->
                        newState
                        |> updateSymbols (GlobalsTable.addTypes [ typeSymbol ] state.Namespace)
                        |> addMember mdef
                        |> Option.map ValidationSuccess
                        |> Option.defaultValue
                            (MemberDef.name mdef
                            |> sprintf "The type %A was unexpectedly in the member stack. Ensure that member sets have been properly pushed and popped from the stack"
                            |> ValidationError)
                    | None -> ValidationError "The member stack was empty"
            | _ ->
                mdef
                |> string
                |> sprintf "The member %s is not a non-nested type or module"
                |> ValidationError)
        |> Validator

    let memberValidator() =
        (fun state mdef ->
            match List.tryHead state.Members with
            | Some members ->
                if members.Contains mdef then
                    mdef
                    |> sprintf "The member %A already exists in the scope"
                    |> ValidationError
                else
                    { state with Members = members.Add(mdef) :: state.Members.Tail }
                    |> ValidationSuccess
            | None -> ValidationError "The member stack was empty")
        |> Validator

type ParserState = ParserState<ParserState.Validator>
