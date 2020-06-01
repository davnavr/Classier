namespace Classier.NET.Compiler.Parsing

open System
open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Decimal
    | Double
    | Float
    | Int32
    | Int64
    | Null
    | UInt32
    | UInt64
    | Unit

    static member Names =
        [
            Boolean, "boolean"
            Decimal, "decimal"
            Double, "double"
            Float, "float"
            Int32, "int"
            Int64, "long"
            Null, "null"
            UInt32, "uint"
            UInt64, "ulong"
        ]
        |> Seq.map KeyValuePair
        |> ImmutableSortedDictionary.CreateRange

    override this.ToString() =
        match this with
        | Unit -> "()"
        | _ -> PrimitiveType.Names.Item this

type TypeName =
    | FuncType of
        {| ParamType: TypeName
           ReturnType: TypeName |}
    | Identifier of Identifier list
    | Inferred
    | Primitive of PrimitiveType
    | Tuple of TypeName list
    | Union of TypeName list

    override this.ToString() =
        match this with
        | FuncType f -> sprintf "%A => %A" (f.ParamType) (f.ReturnType)
        | Identifier names -> String.Join('.', names)
        | Inferred -> "_"
        | Primitive p -> p.ToString()
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
        | Union options -> String.Join(" | ", options)
and Identifier =
    { Name: string
      GenericArgs: GenericArg list }

    override this.ToString() =
        match this.GenericArgs with
        | [] -> this.Name
        | _ ->
            let gargs = String.Join(", ", this.GenericArgs)
            sprintf "%s<%s>" this.Name gargs
and GenericArg = TypeName

module Identifier =
    let ofStrings names =
        names
        |> List.map (fun name ->
            { Name = name
              GenericArgs = [] })
