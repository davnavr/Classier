module rec Classier.NET.Compiler.Extern

open System.Collections.Immutable
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Identifier

type EGenericName = Identifier<GenericParam<EInterface, EClass>>

type EAccess =
    | EPublic
    | EProtected

type EMethod<'Modifier> =
    { MethodName: EGenericName
      Modifiers: 'Modifier
      Parameters: ImmutableArray<ImmutableArray<ExpParam>>
      ReturnType: TypeName }

type EAccessors =
    | EGet
    | ESet
    | EGetSet

type EProperty =
    { Accessors: EAccessors
      PropName: IdentifierStr
      ValueType: TypeName }

type FieldReadonly =
    | IsMutable
    | IsReadonly

type EField =
    { FieldName: IdentifierStr
      Readonly: FieldReadonly
      ValueType: TypeName }

type EFunction =
    { FunctionName: EGenericName
      Parameters: ImmutableArray<ImmutableArray<ExpParam>>
      ReturnType: TypeName }

type EOperator =
    { Kind: OperatorKind
      Operands: ImmutableArray<ExpParam>
      Symbol: OperatorStr
      ReturnType: TypeName }

type EAbstractMember =
    | EAMethod of EMethod<unit>
    | EAProperty of EProperty

type EConcreteMember =
    | EConstructor of ImmutableArray<ExpParam>
    | EMethod of EMethod<MethodModifiers>
    | EProperty of EProperty
    | EField of EField

type EInstanceMember =
    | EAbstract of EAbstractMember
    | EConcrete of EConcreteMember

type EStaticMember =
    | EStaticField of EField
    | EFunction of EFunction
    | EOperator of EOperator

type EMember =
    | EInstanceMember of EInstanceMember
    | EStaticMember of EStaticMember

type EMemberSet<'Member> = ImmutableSortedSet<EAccess * TypeOrMember<EType, 'Member>>

type EInterface<'Parent> =
    { InterfaceName: EGenericName
      Members: EMemberSet<EAbstractMember>
      Parent: 'Parent
      SuperInterfaces: ImmutableSortedSet<EInterface> }

[<RequireQualifiedAccess>]
type EInterface =
    | Nested of EInterface<EType>
    | Global of EInterface<Namespace>

    member this.InterfaceName =
        match this with
        | Nested n -> n.InterfaceName
        | Global g -> g.InterfaceName

type EClass<'Parent> =
    { ClassName: EGenericName
      Interfaces: ImmutableSortedSet<EInterface>
      Members: EMemberSet<EMember>
      Parent: 'Parent
      SuperClass: EClass option }

[<RequireQualifiedAccess>]
type EClass =
    | Nested of EClass<EType>
    | Global of EClass<Namespace>

    member this.ClassName =
        match this with
        | Nested n -> n.ClassName
        | Global g -> g.ClassName

type EModule<'Parent> =
    { ModuleName: EGenericName
      Members: EMemberSet<EStaticMember> }

[<RequireQualifiedAccess>]
type EModule =
    | Nested of EModule<EType>
    | Global of EModule<Namespace>

    member this.ModuleName =
        match this with
        | Nested n -> n.ModuleName
        | Global g -> g.ModuleName

type EType =
    | EClass of EClass
    | EInterface of EInterface
    | EModule of EModule

let typeName =
    function
    | EClass clss -> clss.ClassName
    | EInterface i -> i.InterfaceName
    | EModule m -> m.ModuleName
