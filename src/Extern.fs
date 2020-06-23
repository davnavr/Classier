module Classier.NET.Compiler.Extern

open System.Collections.Immutable
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Grammar.Operator
open Classier.NET.Compiler.Identifier

// TODO: How to differentiate between external members and types that are public or protected?

type EMethod<'Modifier> =
    { MethodName: Identifier
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
    { FunctionName: Identifier
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

type EInterface<'Type> =
    { InterfaceName: Identifier
      Members: ImmutableSortedSet<TypeOrMember<'Type, EAbstractMember>>
      SuperInterfaces: ImmutableSortedSet<EInterface<'Type>> }

type EClass<'Type> =
    { ClassName: Identifier
      Interfaces: ImmutableSortedSet<EInterface<'Type>>
      Members: ImmutableSortedSet<TypeOrMember<'Type, EMember>>
      SuperClass: EClass<'Type> option }

type EModule<'Type> =
    { ModuleName: Identifier
      Members: ImmutableSortedSet<TypeOrMember<'Type, EStaticMember>> }

type EType =
    | EClass of EClass
    | EInterface of EInterface
    | EModule of EModule
and EClass = EClass<EType>
and EInterface = EInterface<EType>
and EModule = EModule<EType>
