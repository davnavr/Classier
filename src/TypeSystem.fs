module Classier.NET.Compiler.TypeSystem

open System
open System.Collections.Generic
open System.Collections.Immutable

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
type Type<'Named> =
    | ArrayType of Type<'Named>
    | FuncType of
        {| ParamType: Type<'Named>
           ReturnType: Type<'Named> |}
    | Named of 'Named
    | Primitive of PrimitiveType
    | Tuple of Type<'Named> list // TODO: Make it so that it requires at least one item?

    override this.ToString() =
        match this with
        | ArrayType itype -> sprintf "%O[]" itype
        | FuncType f -> sprintf "%O => %O" f.ParamType f.ReturnType
        | Named ntype -> ntype.ToString()
        | Primitive p -> string p
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
