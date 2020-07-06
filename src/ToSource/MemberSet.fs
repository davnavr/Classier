namespace Classier.NET.Compiler.ToSource

open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl

type MemberSet =
    | ClassMembers of ClassMembers
    | InterfaceMembers of InterfaceMembers
    | ModuleMembers of ModuleMembers

module MemberSet =
    type private TypeOrMember<'Type, 'Member> =
        Grammar.TypeOrMember<'Type, 'Member>

    let private memberSet ctype cmember omember = // TODO: Simplify this function.
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

    let private paramOverload f =
        function
        | TypeOrMember.Type _ -> List.empty
        | TypeOrMember.Member mdef -> f mdef

    let private tname name = Identifier.noGenerics name |> Some

    let classSet =
        paramOverload
            (fun mdef ->
                match mdef with
                | ClassCtor _
                | _ -> List.empty)
        |> memberSet
            (fun cdef -> tname cdef.ClassName)
            (function
            | _ -> None)

    let interfaceSet =
        paramOverload
            (fun mdef ->
                match mdef with
                | InterfaceMthd _
                | _ -> List.empty)
        |> memberSet
            (fun idef -> tname idef.InterfaceName)
            (function
            | _ -> None)

    let moduleSet =
        paramOverload
            (fun mdef ->
                match mdef with
                | ModuleFunc _
                | _ -> List.empty)
        |> memberSet
            (fun mdle -> Some mdle.ModuleName)
            (function
            | _ -> None)
