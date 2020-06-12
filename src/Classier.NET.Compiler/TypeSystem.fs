module Classier.NET.Compiler.TypeSystem

open System
open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

[<StructuralEquality>]
[<NoComparison>]
[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Null
    | String
    | Unit
    // Integral
    | Byte
    | UByte
    | Short
    | UShort
    | Int
    | UInt
    | Long
    | ULong
    // Floating-point
    | Decimal
    | Double
    | Float

    static member names =
        [
            Boolean, "boolean"
            Null, "null"
            String, "string"

            Byte, "byte"
            UByte, "ubyte"
            Short, "short"
            UShort, "ushort"
            Int, "int"
            UInt, "uint"
            Long, "long"
            ULong, "ulong"

            Decimal, "decimal"
            Double, "double"
            Float, "float"
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
    | Identifier of FullIdentifier<'Generic>
    | Inferred
    | Primitive of PrimitiveType
    | Tuple of TypeName<'Generic> list
    | Union of TypeName<'Generic> list

    override this.ToString() =
        match this with
        | FuncType f -> sprintf "%s => %s" (string f.ParamType) (string f.ReturnType)
        | Identifier names -> String.Join('.', names)
        | Inferred -> "_"
        | Primitive p -> p.ToString()
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
        | Union options -> String.Join(" | ", options)
