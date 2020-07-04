module Classier.NET.Compiler.TypeSystem

open System
open System.Collections.Generic
open System.Collections.Immutable
open Classier.NET.Compiler.Identifier

[<StructuralEquality>]
[<StructuralComparison>]
[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Null
    | String
    | Unit
    // Integral
    | Byte
    | Short
    | Int
    | Long
    // Floating-point
    | Double
    | Float

    static member internal names =
        [
            Boolean, "boolean"
            Null, "null"
            String, "string"

            Byte, "byte"
            Short, "short"
            Int, "int"
            Long, "long"

            Double, "double"
            Float, "float"
        ]
        |> Seq.map KeyValuePair
        |> ImmutableSortedDictionary.CreateRange

    override this.ToString() =
        match this with
        | Unit -> "()"
        | _ -> PrimitiveType.names.Item this

[<StructuralEquality>]
[<StructuralComparison>]
type TypeName<'Named> =
    | ArrayType of TypeName<'Named>
    | FuncType of
        {| ParamType: TypeName<'Named>
           ReturnType: TypeName<'Named> |}
    | Named of 'Named
    | Primitive of PrimitiveType
    | Tuple of TypeName<'Named> list

    override this.ToString() =
        match this with
        | ArrayType itype -> sprintf "%O[]" itype
        | FuncType f -> sprintf "%O => %O" f.ParamType f.ReturnType
        | Named ntype -> ntype.ToString()
        | Primitive p -> string p
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
