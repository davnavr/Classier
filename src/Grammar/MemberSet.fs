namespace Classier.NET.Compiler.Grammar

open System.Collections.Generic
open System.Collections.Immutable

type MemberSet =
    | ClassSet of MemberSet<ClassMember>
    | InterfaceSet of MemberSet<InterfaceMember>
    | ModuleSet of MemberSet<TypeOrMember<TypeDef, StaticMember>>
    | TypeSet of MemberSet<TypeDef>

module MemberSet =
    let private emptySet f =
        ImmutableSortedSet.Empty.WithComparer
            { new IComparer<Access * _> with member _.Compare((_, m1), (_, m2)) = f m1 m2 }

    let private memberSet param typeName memberName typeCompare =
        emptySet
            (fun m1 m2 ->
                match (m1, m2) with
                | (Member i1, Member i2) ->
                    let firstParams m =
                        param m
                        |> Option.map List.tryHead
                        |> Option.flatten
                    let paramCompare =
                        compare
                            (firstParams i1)
                            (firstParams i2)
                    match paramCompare with
                    | 0 ->
                        compare
                            (memberName i1)
                            (memberName i2)
                    | _ -> paramCompare
                | (Member mdef, Type tdef)
                | (Type tdef, Member mdef) ->
                    let factor =
                        match m1 with
                        | Member _ -> 1
                        | Type _ -> -1
                    match param mdef with
                    | Some _ -> factor
                    | None ->
                        let nameCompare =
                            mdef
                            |> memberName
                            |> compare (typeName tdef)
                        factor * nameCompare
                | (Type t1, Type t2) -> typeCompare t1 t2)

    let classSet =
        memberSet
            Member.instanceParams
            (fun cdef -> Some cdef.ClassName)
            Member.instanceName
            (fun c1 c2 -> compare c1.ClassName c2.ClassName)

    let interfaceSet =
        memberSet
            (function
            | AMethod mdef -> Some mdef.Method.Parameters
            | _ -> None)
            (fun idef -> idef.InterfaceName)
            (function
            | AMethod mdef -> mdef.MethodName
            | AProperty pdef -> Name.asGeneric pdef.PropName)
            (fun i1 i2 -> compare i1.InterfaceName i2.InterfaceName)

    let moduleSet =
        memberSet
            (function
            | Function fdef -> Some fdef.Function.Parameters
            | Operator op -> Some [ op.Operands ])
            (fun tdef ->
                match tdef with
                | Class cdef -> cdef.ClassName
                | Interface idef -> idef.InterfaceName
                | Module mdef -> Name.asGeneric mdef.ModuleName
                |> IdentifierName)
            (function
            | Function fdef -> IdentifierName fdef.FunctionName
            | Operator op -> OperatorName op.Symbol)
            TypeDef.compare

    let typeSet = emptySet TypeDef.compare
