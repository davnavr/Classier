module Classier.NET.Compiler.TypeSystem

open System.Collections.Generic
open System.Collections.Immutable

[<StructuralEquality>]
[<StructuralComparison>]
[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Null // TODO: Remove null, since it should not be a type.
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
    | Tuple of Type<'Named> * Type<'Named> * Type<'Named> list

    override this.ToString() =
        match this with
        | ArrayType itype -> sprintf "%O[]" itype
        | FuncType f -> sprintf "%O => %O" f.ParamType f.ReturnType
        | Named ntype -> ntype.ToString()
        | Primitive p -> string p
        | Tuple (i1, i2, rest) ->
            i1 :: i2 :: rest
            |> Seq.map (sprintf "%O")
            |> String.concat ", "
            |> sprintf "(%s)"
