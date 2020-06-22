module Classier.NET.Compiler.GlobalType

open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Extern

[<StructuralEquality>]
[<NoComparison>]
type GlobalType =
    | DefinedType of TypeDef
    | ExternType of EType

type GlobalTypeSymbol =
    { Namespace: FullIdentifier option
      Type: GlobalType }

let getName gtype =
    match gtype with
    | DefinedType tdef ->
        let typeName =
            match tdef with
            | Class cdef -> cdef.ClassName
            | Interface idef -> idef.InterfaceName
            | Module mdef -> Name.asGeneric mdef.ModuleName
        typeName.Identifier
    | ExternType etype ->
        match etype with
        | EClass cdef -> cdef.ClassName
        | EInterface idef -> idef.InterfaceName
        | EModule mdef -> mdef.ModuleName
