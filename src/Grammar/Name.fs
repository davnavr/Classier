namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler
open Classier.NET.Compiler.Identifier

type Name<'Identifier> =
    { Identifier: 'Identifier
      Position: FParsec.Position }

    override this.ToString() = this.Identifier.ToString()

type SimpleName = Name<IdentifierStr>
type GenericName = Name<Identifier>

module Name =
    let asGeneric name =
        { Identifier = Identifier.ofStr name.Identifier
          Position = name.Position }
