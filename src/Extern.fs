namespace Classier.NET.Compiler.Extern

open System.Collections.Immutable
open Classier.NET.Compiler.Generic
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.Grammar.Operator
open Classier.NET.Compiler.Identifier

type EGenericName<'EInterface, 'EClass> = Identifier<GenericParam<'EInterface, 'EClass>>

type EAccess =
    | EPublic
    | EProtected

type EMethod<'Modifier, 'EInterface, 'EClass> =
    { MethodName: EGenericName<'EInterface, 'EClass>
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

type EFunction<'EInterface, 'EClass> =
    { FunctionName: EGenericName<'EInterface, 'EClass>
      Parameters: ImmutableArray<ImmutableArray<ExpParam>>
      ReturnType: TypeName }

type EOperator =
    { Kind: OperatorKind
      Operands: ImmutableArray<ExpParam>
      Symbol: OperatorStr
      ReturnType: TypeName }

type EAbstractMember<'EInterface, 'EClass> =
    | EAMethod of EMethod<unit, 'EInterface, 'EClass>
    | EAProperty of EProperty

type EConcreteMember<'EInterface, 'EClass> =
    | EConstructor of ImmutableArray<ExpParam>
    | EMethod of EMethod<MethodModifiers, 'EInterface, 'EClass>
    | EProperty of EProperty
    | EField of EField

type EInstanceMember<'EInterface, 'EClass> =
    | EAbstract of EAbstractMember<'EInterface, 'EClass>
    | EConcrete of EConcreteMember<'EInterface, 'EClass>

type EStaticMember<'EInterface, 'EClass> =
    | EStaticField of EField
    | EFunction of EFunction<'EInterface, 'EClass>
    | EOperator of EOperator

type EMember<'EInterface, 'EClass> =
    | EInstanceMember of EInstanceMember<'EInterface, 'EClass>
    | EStaticMember of EStaticMember<'EInterface, 'EClass>

type EMemberSet<'Type, 'Member> = ImmutableSortedSet<EAccess * TypeOrMember<'Type, 'Member>>

type EInterface<'EType> =
    { InterfaceName: EGenericName<EInterface<'EType>, EClass<'EType>>
      Members: EMemberSet<'EType, EAbstractMember<EInterface<'EType>, EClass<'EType>>>
      SuperInterfaces: ImmutableSortedSet<EInterface<'EType>> }
and EClass<'EType> =
    { ClassName: EGenericName<EInterface<'EType>, EClass<'EType>>
      Interfaces: ImmutableSortedSet<EInterface<'EType>>
      Members: EMemberSet<'EType, EMember<EInterface<'EType>, EClass<'EType>>>
      SuperClass: EClass<'EType> option }
and EModule<'EType> =
    { ModuleName: EGenericName<EInterface<'EType>, EClass<'EType>>
      Members: EMemberSet<'EType, EStaticMember<EInterface<'EType>, EClass<'EType>>> }

type EType =
    | EClass of EClass
    | EInterface of EInterface
    | EModule of EModule
and EClass = EClass<EType>
and EInterface = EInterface<EType>
and EModule = EModule<EType>

module EType =
    let name etype =
        match etype with
        | EClass c -> c.ClassName
        | EInterface i -> i.InterfaceName
        | EModule m -> m.ModuleName
