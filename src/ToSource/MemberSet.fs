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

    let classSet =
        memberSet
            (fun cdef -> cdef.ClassName)
            (function
            | _ -> None)
            (fun mdef ->
                match mdef with
                | ClassCtor _
                | _ -> List.empty)

    let interfaceSet =
        memberSet
            (fun idef -> idef.InterfaceName)
            (function
            | _ -> None)
            (fun mdef ->
                match mdef with
                | InterfaceMthd _
                | _ -> List.empty)

    let moduleSet =
        memberSet
            (function
            | GenClass clss -> clss.ClassName
            | GenInterface intf -> intf.InterfaceName
            | GenModule mdle ->
                Identifier.ofStr mdle.ModuleName)
            (function
            | _ -> None)
            (fun mdef ->
                match mdef with
                | ModuleFunc _
                | _ -> List.empty)
