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

type EInterface =
    { InterfaceName: EGenericName
      Members: EMemberSet<EAbstractMember>
      SuperInterfaces: ImmutableSortedSet<EInterface> }

type EClass =
    { ClassName: EGenericName
      Interfaces: ImmutableSortedSet<EInterface>
      Members: EMemberSet<EMember>
      SuperClass: EClass option }

type EModule =
    { ModuleName: EGenericName
      Members: EMemberSet<EStaticMember> }

type EType =
    | EClass of EClass
    | EInterface of EInterface
    | EModule of EModule

let typeName etype =
    match etype with
    | EClass c -> c.ClassName
    | EInterface i -> i.InterfaceName
    | EModule m -> m.ModuleName
