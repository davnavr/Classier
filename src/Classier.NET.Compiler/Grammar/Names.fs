namespace Classier.NET.Compiler.Grammar

open System
open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Null
    | Numeric of NumType
    | String
    | Unit

    static member names =
        [
            Boolean, "boolean"
            Null, "null"
            Numeric NumType.Decimal, "decimal"
            Numeric NumType.Double, "double"
            Numeric NumType.Float, "float"
            Numeric NumType.Integer, "int"
            Numeric NumType.Long, "long"
            Numeric (NumType.Integer ||| NumType.Unsigned), "uint"
            Numeric (NumType.Long ||| NumType.Unsigned), "ulong"
            String, "string"
        ]
        |> Seq.map KeyValuePair
        |> ImmutableSortedDictionary.CreateRange

    override this.ToString() =
        match this with
        | Unit -> "()"
        | _ -> PrimitiveType.names.Item this

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
and [<StructuralComparison>]
    [<StructuralEquality>]
    Identifier =
    { Name: string
      Generics: Generic list }

    override this.ToString() =
        match this.Generics with
        | [] -> this.Name
        | _ ->
            String.Join(", ", this.Generics)
            |> sprintf "%s<%s>" this.Name
and Generic =
    | GenericArg of TypeName
    | GenericParam of GenericParam

    override this.ToString() =
        match this with
        | GenericArg name -> name.ToString()
        | GenericParam param -> param.ToString()
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
