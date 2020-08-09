namespace Classier.NET.Compiler.IR

open Classier.NET.Compiler.Grammar

module GenParam =
    let ofExpParam eparam tresolver =
        { Name = eparam.Name
          Syntax = eparam
          Type = tresolver eparam.Type }
