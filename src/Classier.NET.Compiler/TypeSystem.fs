module Classier.NET.Compiler.TypeSystem

open System
open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Null
    //| Numeric of NumType
    | String
    | Unit

    static member names =
        [
            Boolean, "boolean"
            Null, "null"
            //Numeric NumType.Decimal, "decimal"
            //Numeric NumType.Double, "double"
            //Numeric NumType.Float, "float"
            //Numeric NumType.Integer, "int"
            //Numeric NumType.Long, "long"
            //Numeric (NumType.Integer ||| NumType.Unsigned), "uint"
            //Numeric (NumType.Long ||| NumType.Unsigned), "ulong"
            String, "string"
        ]
        |> Seq.map KeyValuePair
        |> ImmutableSortedDictionary.CreateRange

    override this.ToString() =
        match this with
        | Unit -> "()"
        | _ -> PrimitiveType.names.Item this

type TypeName<'Generic> =
    | FuncType of
        {| ParamType: TypeName<'Generic>
           ReturnType: TypeName<'Generic> |}
    | Identifier of Identifier<'Generic> list
    | Inferred
    | Primitive of PrimitiveType
    | Tuple of TypeName<'Generic> list
    | Union of TypeName<'Generic> list

    override this.ToString() =
        match this with
        | FuncType f -> sprintf "%A => %A" (f.ParamType) (f.ReturnType)
        | Identifier names -> String.Join('.', names)
        | Inferred -> "_"
        | Primitive p -> p.ToString()
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
        | Union options -> String.Join(" | ", options)
