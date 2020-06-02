namespace Classier.NET.Compiler.Grammar

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
    | String
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
            String, "string"
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
      Generics: Generic list }

    override this.ToString() =
        match this.Generics with
        | [] -> this.Name
        | _ ->
            let gargs = String.Join(", ", this.Generics)
            sprintf "%s<%s>" this.Name gargs
and Generic =
    | GenericArg of TypeName
    | GenericParam of GenericParam
and GenericVariance =
    | NoVariance
    | Covariant
    | Contravariant
and GenericParam =
    { Name: string
      RequiredSuperClass: Identifier list
      RequiredInterfaces: TypeName list
      Variance: GenericVariance }

    override this.ToString() = this.Name

module Identifier =
    let ofString name = { Name = name; Generics = [] }

    let ofStrings names =
        names
        |> List.map (fun name ->
            { Name = name
              Generics = [] })
