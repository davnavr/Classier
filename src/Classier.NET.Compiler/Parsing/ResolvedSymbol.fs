namespace Classier.NET.Compiler.Parsing

type ResolvedSymbol =
    { Origin: SymbolOrigin
      Parent: ResolvedSymbol list
      Symbol: Symbol }

module ResolvedSymbol =
    let ofNamespace names pos =
        { Origin = SourceCode pos
          Parent = []
          Symbol = Symbol.Namespace names }
