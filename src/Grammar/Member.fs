[<RequireQualifiedAccess>]
module Classier.NET.Compiler.Grammar.Member

open Classier.NET.Compiler.AccessControl

let inline private methodParams map def =
    (^a : (member Method : Signature<_, _>) def).Parameters
    |> List.map (List.map map)
    |> Some

let withAccess<'Member> (acc: Access) (mdef: 'Member) = acc, mdef
let defaultAccess<'Member> = withAccess<'Member> Access.Public

let defaultCtor =
    { Call = NullLit
      Parameters = List.empty
      SelfIdentifier = None }

let foldInstance amthd aprop ctor mthd prop mdef =
    match mdef with
    | Abstract adef ->
        match adef with
        | AMethod mdef -> amthd mdef
        | AProperty pdef -> aprop pdef
    | Concrete cdef ->
        match cdef with
        | Constructor c -> ctor c
        | Method mdef -> mthd mdef
        | Property pdef -> prop pdef

let instanceParams =
    let emptyParams _ = None
    foldInstance
        (methodParams Param.asInferred)
        emptyParams
        (fun ctor -> Some [ ctor.Parameters ])
        (methodParams id)
        emptyParams

let instanceName =
    let inline methodName def = Some ((^a) : (member MethodName : GenericName) (def))
    let inline propName def =
        ((^a) : (member PropName : SimpleName) (def))
        |> Name.asGeneric
        |> Some
    foldInstance
        methodName
        propName
        (fun _ -> None)
        methodName
        propName

let instanceSig mdef =
    match mdef with
    | Abstract adef ->
        match adef with
        | AMethod mthd ->
            mthd.Method.Parameters
            |> Seq.map Param.toExpStr
            |> String.concat " "
            |> sprintf "%O%s" mthd.MethodName
        | AProperty prop -> string prop.PropName
    | Concrete cdef ->
        match cdef with
        | Constructor ctor ->
            ctor.Parameters
            |> Param.toInfStr
            |> sprintf "new%s"
        | Method mthd ->
            mthd.Method.Parameters
            |> Seq.map Param.toInfStr
            |> String.concat " "
            |> sprintf "%O%s" mthd.MethodName
        | Property prop -> string prop.PropName
