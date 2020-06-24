module Classier.NET.Compiler.Namespace

open System
open Classier.NET.Compiler.Grammar

type Namespace =
    | Namespace of FullIdentifier
    | GlobalNamespace

    override this.ToString() =
        match this with
        | Namespace ns -> string ns
        | GlobalNamespace -> String.Empty

let fullId ns =
    match ns with
    | Some fullId -> Namespace fullId
    | None -> GlobalNamespace
