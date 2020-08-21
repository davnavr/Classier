[<AutoOpen>]
module rec Classier.NET.Compiler.IR.Types

open System.Collections.Immutable

open Classier.NET.Compiler
open Classier.NET.Compiler.AccessControl
open Classier.NET.Compiler.Generic

module Ast = Classier.NET.Compiler.Grammar.Ast

type GenOrRef<'Gen, 'Ref> =
    | Gen of 'Gen
    | Ref of 'Ref

type GlobalOrNested<'Global, 'Nested> =
    | Global of 'Global
    | Nested of 'Nested

type ResolvedType = TypeSystem.Type<NamedType>

type GenParam =
    { Name: IdentifierStr option
      Syntax: Ast.ExpParam
      Type: ResolvedType }

type GenParamTuple = ImmutableList<GenParam>
type GenParamList = ImmutableList<GenParamTuple>

type GenName = Identifier<GenericParam<GenOrRef<GenInterface, RefInterface>, GenOrRef<GenClass, RefClass>>>
type RefName = Identifier<GenericParam<RefInterface, RefClass>>

type GenSignature<'Body> =
    { Body: 'Body
      Parameters: GenParamList
      ReturnType: ResolvedType }

type CallExpression<'Target> =
    { Arguments: ImmutableList<GenExpression>
      Target: 'Target }

type MemberAccess<'Target, 'Member> =
    { Member: 'Member
      Target: 'Target }

type ComplexExpression =
    | CtorCall of CallExpression<GenClass>
    | ClassMemberAccess of MemberAccess<GenExpression, GenClassMember>
    | ModuleMemberAccess of MemberAccess<GenModule, GenModuleMember>
type GenExpression =
    | BoolLit of bool
    | ComplexExpr of ComplexExpression
    | StrLit of string

type GenStatement =
    | Empty
    | IgnoredExpr of GenExpression

type GenBody = ImmutableList<GenStatement * Ast.PStatement>

type GenPrimaryCtor =
    { Body: GenBody
      Parameters: GenParamTuple
      ParentClass: GenClass
      Syntax: Ast.PrimaryCtor }

type PrimaryCtorCall =
    | PrimaryCtorCall of ImmutableList<GenExpression>

type GenCtor =
    { Parameters: GenParamTuple
      ParentClass: GenClass
      SelfCall: PrimaryCtorCall
      Syntax: Ast.Ctor }

type GenMethod<'Parent, 'Body, 'Syntax> =
    { MethodName: GenName
      Parent: 'Parent
      Signature: GenSignature<'Body>
      Syntax: 'Syntax }

type GenAbstractMethod<'Parent> = GenMethod<'Parent, unit, Ast.AbstractMethod>
type GenConcreteMethod<'Parent> = GenMethod<'Parent, GenBody, Ast.Method>

type GenMethod<'Parent> =
    | GenAbstractMethod of GenAbstractMethod<'Parent>
    | GenConcreteMethod of GenConcreteMethod<'Parent>

type GenPropertyAccessors = unit

type GenProperty<'Parent, 'Body, 'Syntax> =
    { Accessors: GenPropertyAccessors
      Parent: 'Parent
      PropName: GenName
      Syntax: Ast.Property
      ValueType: ResolvedType }

type GenAbstractProperty<'Parent> = GenProperty<'Parent, unit, Ast.AbstractProperty>
type GenConcreteProperty<'Parent> = GenProperty<'Parent, GenBody, Ast.Property>

type GenProperty<'Parent> =
    | GenAbstractProperty of GenAbstractProperty<'Parent>
    | GenConcreteProperty of GenConcreteProperty<'Parent>

type GenFunction<'Parent, 'Body, 'Syntax> = // TODO: Maybe rename, since it is used as both a ref or gen function.
    { FunctionName: GenName
      ParentModule: 'Parent
      Signature: GenSignature<'Body>
      Syntax: 'Syntax }

type GenOperatorKind =
    | InfixOp of GenSignature<GenBody>
    | PrefixOp of
        {| Body: GenBody
           Operand: GenParam
           ReturnType: ResolvedType |}

type GenOperator =
    { OperatorKind: GenOperatorKind
      ParentModule: GenModule
      Syntax: Ast.Operator }

type MemberSet<'Member> = ImmutableSortedSet<Access * 'Member>
type GenInterfaceSet = ImmutableSortedSet<GenOrRef<GenInterface, RefInterface>>
type RefInterfaceSet = ImmutableSortedSet<RefInterface>

type GenClassMember =
    | ClassCtor of GenCtor
    | ClassMthd of GenMethod<GenClass>
    | ClassProp of GenProperty<GenClass>
    | NestedClass of GenNestedClass

type GenClass<'Parent> =
    { ClassName: GenName
      Interfaces: GenInterfaceSet
      Members: MemberSet<GenClassMember>
      Parent: 'Parent
      PrimaryCtor: GenPrimaryCtor
      SuperClass: GenClass option
      Syntax: Ast.Class }

type GenNestedClass = GenClass<GenClass>
type GenGlobalClass = GenClass<Namespace>

[<RequireQualifiedAccess>]
type GenClass =
    | Nested of GenNestedClass
    | Global of GenGlobalClass

    member this.ClassName =
        match this with
        | Nested n -> n.ClassName
        | Global g -> g.ClassName

type GenInterfaceMember =
    | InterfaceMthd of GenAbstractMethod<GenInterface>
    | InterfaceProp of GenAbstractProperty<GenInterface>
    | NestedInterface of GenNestedInterface

type GenInterface<'Parent> =
    { InterfaceName: GenName
      Members: MemberSet<GenInterfaceMember>
      Parent: 'Parent
      SuperInterfaces: GenInterfaceSet
      Syntax: Ast.Interface }

type GenNestedInterface = GenInterface<GenInterface>
type GenGlobalInterface = GenInterface<Namespace>

[<RequireQualifiedAccess>]
type GenInterface =
    | Nested of GenNestedInterface // TODO: Maybe create NestedOrGlobal<_, _> type?
    | Global of GenGlobalInterface

    member this.InterfaceName =
        match this with
        | Nested n -> n.InterfaceName
        | Global g -> g.InterfaceName

type GenModuleMember =
    | ModuleFunc of GenFunction<GenModule, GenBody, Ast.StaticFunction>
    | ModuleOper of GenOperator
    | NestedDecl of GenNestedDecl<GenModule>

type GenModule<'Parent> =
    { Members: MemberSet<GenModuleMember>
      ModuleName: IdentifierStr
      Parent: 'Parent
      Syntax: Ast.Module }

type GenNestedModule = GenModule<GenModule>
type GenGlobalModule = GenModule<Namespace>

[<RequireQualifiedAccess>]
type GenModule =
    | Nested of GenNestedModule
    | Global of GenGlobalModule

type RefClassMember =
    | RefCtor
    // TODO: Add more members

type RefClass<'Parent> =
    { ClassName: RefName
      Interfaces: RefInterfaceSet
      Members: ImmutableSortedSet<PublicAccess * RefClassMember>
      Parent: 'Parent }

type RefNestedClass = RefClass<RefClass>
type RefGlobalClass = RefClass<Namespace>

[<RequireQualifiedAccessAttribute>]
type RefClass =
    | Global of RefGlobalClass

type RefInterfaceMember = unit

type RefInterface<'Parent> =
    { InterfaceName: RefName
      Members: ImmutableSortedSet<RefInterfaceMember>
      Parent: 'Parent
      SuperInterfaces: RefInterfaceSet
      Syntax: Ast.ExternInterface }

type RefNestedInterface = RefInterface<RefInterface>
type RefGlobalInterface = RefInterface<Namespace>

type RefInterface =
    | Nested of RefNestedInterface
    | Global of RefGlobalInterface

type RefModuleMember =
    | RefFunc of GenFunction<RefModule, unit, Ast.ExternFunction>
    | RefNestedDecl of RefNestedDecl

type RefModule<'Parent> =
    { Members: ImmutableSortedSet<RefModuleMember>
      ModuleName: IdentifierStr
      Parent: 'Parent
      Syntax: Ast.ExternModule }

type RefNestedModule = RefModule<RefModule>
type RefGlobalModule = RefModule<Namespace>

[<RequireQualifiedAccess>]
type RefModule =
    | Nested of RefNestedModule
    | Global of RefGlobalModule

type NamedType =
    | GenClass of GenClass
    | GenInterface of GenInterface
    | RefClass of RefClass
    | RefInterface of RefInterface

type GenNestedDecl<'Parent> =
    | GenNestedClass of GenClass<'Parent>
    | GenNestedInterface of GenInterface<'Parent>
    | GenNestedModule of GenModule<'Parent>

type GenGlobalDecl =
    | GenGlobalClass of GenGlobalClass
    | GenGlobalInterface of GenGlobalInterface
    | GenGlobalModule of GenGlobalModule

type RefNestedDecl =
    | RefNestedClass of RefNestedClass
    | RefNestedInterface of RefNestedInterface
    | RefNestedModule of RefNestedModule

type RefGlobalDecl =
    | RefGlobalClass of RefGlobalClass
    | RefGlobalInterface of RefGlobalInterface
    | RefGlobalModule of RefGlobalModule

type GlobalDecl = GenOrRef<GenGlobalDecl, RefGlobalDecl>

type ExitCode =
    | Explicit
    | ImplicitZero

type GenEntryPoint =
    { ExitCode: ExitCode
      Parameters: GenParamTuple
      Body: GenBody
      Syntax: Ast.EntryPoint }

type GenOutput =
    { GlobalDecls: seq<GenGlobalDecl>
      EntryPoint: GenEntryPoint option }
