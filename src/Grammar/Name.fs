namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

type TypeName = TypeSystem.TypeName<Generic.Generic>

type Name<'Identifier> =
    { Identifier: 'Identifier
      Position: FParsec.Position }

    override this.ToString() = this.Identifier.ToString()

type SimpleName = Name<IdentifierStr>
type GenericName = Name<Identifier<Generic.GenericParam>>

module Name =
    let simple pos (str: IdentifierStr) =
        { Identifier = str
          Position = pos }

    let ofStr pos str =
        { Identifier = Identifier.ofStr str
          Position = pos }

    let asGeneric name = ofStr name.Position name.Identifier
