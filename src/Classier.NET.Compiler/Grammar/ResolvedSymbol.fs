namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

type ResolvedSymbol =
    { Origin: SymbolOrigin
      Parents: ResolvedSymbol list
      Symbol: Symbol }

module ResolvedSymbol =
    let ofNamespace names pos =
        { Origin = SourceCode pos
          Parents = List.empty // TODO: This should be set to the parent namespace.
          Symbol = Symbol.Namespace names }
    let ofType typeDef parents =
        { Origin = SourceCode typeDef.Definition.Position
          Parents = parents
          Symbol = Symbol.Type typeDef }
