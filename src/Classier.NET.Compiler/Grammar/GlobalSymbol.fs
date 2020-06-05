namespace Classier.NET.Compiler.Grammar

type GlobalSymbol =
    { Origin: SymbolOrigin
      Parents: GlobalSymbol list
      Symbol: Symbol }

module GlobalSymbol =
    let ofNamespace names pos =
        { Origin = SourceCode pos
          Parents = List.empty
          Symbol = Symbol.Namespace names }
    let ofType typeDef parents =
        { Origin = SourceCode typeDef.Definition.Position
          Parents = parents
          Symbol = Symbol.Type typeDef }
