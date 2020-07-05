namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

type TypeParam = // TODO: Come up with better name for these three types, and maybe move them to a separate file?
    | TypeParam of GenericParam<FullIdentifier<TypeArgOrParam>, FullIdentifier<TypeArgOrParam>>
and TypeArgOrParam = Generic<TypeName, FullIdentifier<TypeParam>, FullIdentifier<TypeParam>>
and TypeName =
    | TypeName of Type<FullIdentifier<TypeArgOrParam>>

type Name<'Identifier> =
    { Identifier: 'Identifier
      Position: FParsec.Position }

    override this.ToString() = this.Identifier.ToString()

type SimpleName = Name<IdentifierStr>
type GenericName = Name<Identifier<TypeParam>>

module Name =
    let simple pos (str: IdentifierStr) =
        { Identifier = str
          Position = pos }

    let ofStr pos str =
        { Identifier = Identifier.ofStr str
          Position = pos }

    let asGeneric name = ofStr name.Position name.Identifier
