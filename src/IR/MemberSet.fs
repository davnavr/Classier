module Classier.NET.Compiler.IR.MemberSet

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl

let private memberSet tname mname mparams =
    { new IComparer<Access * _> with
        member _.Compare((_, m1), (_, m2)) =
            let ctype =
                tname
                >> Identifier.noGenerics
                >> Some
            let oload =
                function
                | TypeOrMember.Type _ ->
                    List.empty
                | TypeOrMember.Member mdef ->
                    mparams mdef
            let overload =
                compare
                    (oload m1)
                    (oload m2)
            match (overload, m1, m2) with
            | (0, TypeOrMember.Member i1, TypeOrMember.Member i2) ->
                compare (mname i1) (mname i2)
            | (0, TypeOrMember.Type t1, TypeOrMember.Type t2) ->
                compare (ctype t1) (ctype t2)
            | (0, TypeOrMember.Member mdef, TypeOrMember.Type tdef)
            | (0, TypeOrMember.Type tdef, TypeOrMember.Member mdef) ->
                let factor =
                    match m1 with
                    | TypeOrMember.Member _ -> 1
                    | TypeOrMember.Type _ -> -1
                compare (ctype tdef) (mname mdef) * factor
            | _ -> overload }
    |> ImmutableSortedSet.Empty.WithComparer

let emptyClass =
    memberSet
        (fun cdef -> cdef.ClassName)
        (function
        | _ -> None)
        (fun mdef ->
            match mdef with
            | ClassCtor _
            | _ -> List.empty)

let emptyInterface =
    memberSet
        (fun idef -> idef.InterfaceName)
        (function
        | _ -> None)
        (fun mdef ->
            match mdef with
            | InterfaceMthd _
            | _ -> List.empty)

let emptyModule =
    memberSet
        GenType.name
        (function
        | _ -> None)
        (fun mdef ->
            match mdef with
            | ModuleFunc _
            | _ -> List.empty)

let rec ofClass (gclass: GenClass) = // TODO: Should this take a GenClass or a Grammar.Ast.Class?
    let nextmem acc (macc, mdef) =
        let gmember = // TODO: Maybe have functions elsewhere that handle creation of placeholder members?
            match mdef with
            | TypeOrMember.Type nested ->
                nested
                |> GenType.gclass
                    ImmutableSortedSet.Empty // TODO: Create way to handle creation of interface sets.
                    emptyClass
                |> ofClass
                |> ignore
                //|> TypeOrMember.Type
                invalidOp "bad"
            | TypeOrMember.Member mdef ->
                invalidOp "bad"
        Result.bind
            (fun (set: ImmutableSortedSet<_>) -> // TODO: Error can also occur if mdef is a nested class with invalid members, since ofClass needs to be called recursively.
                let add =
                    SortedSet.tryAdd (macc, gmember) set
                match add with
                | Some nset -> Result.Ok nset
                | None -> Result.Error gmember)
            acc
    Seq.fold
        (fun acc (macc, mdef) ->
            result {
                let! set = acc
                return set
            })
        (Result.Ok emptyClass)
        gclass.Syntax.Members
