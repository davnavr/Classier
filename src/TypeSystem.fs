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
type TypeName<'Generic> = // TODO: Make this TypeName<'NamedType> instead.
    | ArrayType of TypeName<'Generic>
    | FuncType of
        {| ParamType: TypeName<'Generic>
           ReturnType: TypeName<'Generic> |}
    | Named of FullIdentifier<'Generic> // TODO: Replace case with a case named 'Defined', and add a corresponding generic parameter 'Defined?
    | Primitive of PrimitiveType
    | Tuple of TypeName<'Generic> list

    override this.ToString() =
        match this with
        | ArrayType itype -> sprintf "%O[]" itype
        | FuncType f -> sprintf "%O => %O" f.ParamType f.ReturnType
        | Named ntype -> string ntype
        | Primitive p -> p.ToString()
        | Tuple types -> sprintf "(%s)" (String.Join(", ", types))
