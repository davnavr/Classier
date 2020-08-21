[<RequireQualifiedAccess>]
module Classier.NET.Compiler.IR.MemberSet

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl

let private memberSet tname mname mparams =
    let mcompare (_: Access, m1) (_, m2) =
        let ctype =
            tname
            >> Identifier.simplify
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
        | _ -> overload
    SortedSet.withComparer
        mcompare
        ImmutableSortedSet.Empty

let emptyClass: MemberSet<GenClassMember> = invalidOp "bad"

let emptyInterface: MemberSet<GenInterfaceMember> = invalidOp "bad"

let emptyModule: MemberSet<GenModuleMember> = invalidOp "bad"
