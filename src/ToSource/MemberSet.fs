namespace Classier.NET.Compiler.ToSource

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl

type MemberSet =
    | ClassMembers of ClassMembers<GenType>
    | InterfaceMembers of InterfaceMembers<GenType>
    | ModuleMembers of ModuleMembers<GenType>

module MemberSet =
    let private memberSet ctype cmember omember =
        { new IComparer<Access * _> with
              member _.Compare((_, m1), (_, m2)) =
                  let overload =
                      compare
                          (omember m1)
                          (omember m2)
                  match (overload, m1, m2) with
                  | (0, TypeOrMember.Member i1, TypeOrMember.Member i2) ->
                      compare (cmember i1) (cmember i2)
                  | (0, TypeOrMember.Type t1, TypeOrMember.Type t2) ->
                      compare (ctype t1) (ctype t2)
                  | (0, TypeOrMember.Member mdef, TypeOrMember.Type tdef)
                  | (0, TypeOrMember.Type tdef, TypeOrMember.Member mdef) ->
                      let factor =
                          match m1 with
                          | TypeOrMember.Member _ -> 1
                          | TypeOrMember.Type _ -> -1
                      compare (ctype tdef) (cmember mdef) * factor
                  | _ -> overload }
        |> ImmutableSortedSet.Empty.WithComparer

    let classSet =
        memberSet
            (fun cdef -> Some cdef.ClassName)
            (function
            | _ -> None)
            (function
            | TypeOrMember.Type _ -> List.empty
            | TypeOrMember.Member mdef ->
                match mdef with
                | ClassCtor ctor -> ctor.Parameters; invalidOp "bad"
                | _ -> List.empty)
