namespace rec Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Identifier
open Classier.NET.Compiler.TypeSystem

type TypeParam =
    | TypeParam of GenericParam<FullIdentifier<TypeArg>, FullIdentifier<TypeArg>>

    override this.ToString() =
        let (TypeParam id) = this
        id.ToString()

type TypeArg = Generic<TypeName, FullIdentifier<TypeParam>, FullIdentifier<TypeParam>>

type TypeName =
    | TypeName of Type<FullIdentifier<TypeArg>>

module TypeParam =
    let identifierMap =
        mapGenerics
            (fun (TypeParam tparam) -> tparam)
