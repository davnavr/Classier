[<RequireQualifiedAccess>]
module Classier.NET.Compiler.Assert

open System.Collections

let inline fail msg = msg |> Fuchu.AssertException |> raise
let inline failf format = Printf.ksprintf fail format

let isSome opt =
    match opt with
    | Some value -> value
    | None ->
        fail "The value was unexpectedly None"
let isTrue msg value =
    if not value then
        fail msg
let it check msg it =
    if check it
    then it
    else fail msg

let isOk =
    function
    | Result.Ok ok -> ok
    | Result.Error err ->
        failf "The value was an error: %O" err
let isError =
    function
    | Result.Error err -> err
    | Result.Ok r -> failf "Unexpected success: %O" r

let equal (exp: 'T when 'T: equality) act =
    let format (item: obj) =
        match item with
        | :? string as str ->
            sprintf "\"%s\"" str
        | :? IEnumerable as items ->
            let col = Seq.cast<obj> items
            let content =
                if Seq.isEmpty col
                then "(empty)"
                else
                    col
                    |> Seq.map string
                    |> String.concat ",\n"
            sprintf "%O:\n%s" typeof<'T> content
        | _ -> item.ToString()
    match exp = act with
    | true -> act
    | false ->
        failf
            "Expected:\n%s\nActual:\n%s"
            (format exp)
            (format act)

let head s =
    match Seq.tryHead s with
    | Some h -> h
    | None ->
        fail "The head of the sequence could not be retrieved since the sequence is empty"
let notEmpty col =
    if Seq.isEmpty col
    then fail "The collection was unexpectedly empty"
    else col

let strContains (sub: string) (str: string) =
    if str.Contains sub
    then str
    else failf "The substring '%s' in string '%s' could not be found" sub str
