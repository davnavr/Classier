[<RequireQualifiedAccess>]
module Classier.NET.Compiler.Grammar.Member

open Classier.NET.Compiler.AccessControl

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
        | AbstractMethod mdef -> amthd mdef
        | AbstractProperty pdef -> aprop pdef
    | Concrete cdef ->
        match cdef with
        | Constructor c -> ctor c
        | Method mdef -> mthd mdef
        | Property pdef -> prop pdef

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
        | AbstractMethod mthd ->
            mthd.Method.Parameters
            |> Seq.map Param.toExpStr
            |> String.concat " "
            |> sprintf "%O%s" mthd.MethodName
        | AbstractProperty prop -> string prop.PropName
    | Concrete cdef ->
        match cdef with
        | Constructor ctor ->
            ctor.Parameters
            |> Param.toExpStr
            |> sprintf "new%s"
        | Method mthd ->
            mthd.Method.Parameters
            |> Seq.map Param.toExpStr
            |> String.concat " "
            |> sprintf "%O%s" mthd.MethodName
        | Property prop -> string prop.PropName
