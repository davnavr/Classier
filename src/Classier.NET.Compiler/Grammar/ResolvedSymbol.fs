namespace Classier.NET.Compiler.Grammar

open System.Collections.Immutable

type ResolvedSymbol =
    { Origin: SymbolOrigin
      Parents: ImmutableStack<ResolvedSymbol>
      Symbol: Symbol }

module ResolvedSymbol =
    let ofNamespace names pos =
        { Origin = SourceCode pos
          Parents = ImmutableStack.Empty // TODO: This should be set to the parent namespace.
          Symbol = Symbol.Namespace names }
