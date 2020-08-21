namespace Classier.NET.Compiler

open System
open System.Text.RegularExpressions

[<StructuralComparison; StructuralEquality>]
type IdentifierStr =
    private
    | IdentifierStr of string

    override this.ToString() =
        let (IdentifierStr str) = this
        str

[<StructuralComparison; StructuralEquality>]
type Identifier<'Generic> =
    { Name: IdentifierStr
      Generics: 'Generic list }

    override this.ToString() =
        let name = string this.Name
        match this.Generics with
        | [] -> name
        | _ ->
            String.Join(", ", this.Generics)
            |> sprintf "%s<%s>" name

[<StructuralComparison; StructuralEquality>]
type FullIdentifier<'Generic> =
    | FullIdentifier of Identifier<'Generic> * Identifier<'Generic> list

    override this.ToString() =
        let (FullIdentifier(head, ids)) = this
        match ids with
        | [] -> string head
        | _ ->
            String.Join('.', ids)
            |> sprintf "%O.%s" head

[<RequireQualifiedAccess>]
module IdentifierStr =
    let private nameRegex =
        "^[A-Za-z][A-Za-z_0-9]*$" |> Regex

    let private createHelper ok err str =
        match str with
        | Regex.Matches nameRegex ->
            IdentifierStr str |> ok
        | _ -> err str
    let create =
        createHelper
            id
            (sprintf "'%s' is an invalid identifier" >> invalidArg "str")
    let tryCreate =
        createHelper Some (fun _ -> None)

[<RequireQualifiedAccess>]
module Identifier =
    let ofStr str =
        { Name = str
          Generics = List.empty }

    let create str =
        IdentifierStr.create str |> ofStr

    let map gmapper idf =
        { Name = idf.Name
          Generics = List.map gmapper idf.Generics }

    let simplify idf = map (fun _ -> ()) idf
    let (|Simplify|) idf = simplify idf

    let ncompare idf1 idf2 = compare (simplify idf1) (simplify idf2)

    let toFull idf = FullIdentifier(idf, List.empty)

[<RequireQualifiedAccess>]
module FullIdentifier =
    let ofStrs strs =
        Seq.tryHead strs
        |> Option.map
            (fun head ->
                let rest =
                    strs
                    |> Seq.map Identifier.ofStr
                    |> Seq.toList
                FullIdentifier(Identifier.ofStr head, rest))

    let map gmapper (FullIdentifier(head, tail)) =
        let nhead =
            Identifier.map gmapper head
        let ntail =
            List.map (Identifier.map gmapper) tail
        FullIdentifier(nhead, ntail)

    let toList (FullIdentifier (head, tail)) = head :: tail

    let (|Namespace|_|) name =
        let nlist = toList name
        let strs() = List.map (fun id -> id.Name) nlist
        List.forall
            (fun idf -> List.isEmpty idf.Generics)
            nlist
        |> Bool.toOpt
        |> Option.map strs
