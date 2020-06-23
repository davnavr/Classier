namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type MutatorModf =
    | IsMutator
    | IsPure

type Ctor =
    { Body: Statement list
      Parameters: InfParam list
      SelfIdentifier: IdentifierStr option }

    override this.ToString() =
        sprintf "new %s" (Param.toInfStr this.Parameters)

[<RequireQualifiedAccess>]
type MethodImpl =
    | Default
    | Override
    | SealedOverride
    | Virtual

type MethodModifiers =
    { ImplKind: MethodImpl
      Purity: MutatorModf }

    static member Default =
        { ImplKind = MethodImpl.Default
          Purity = IsMutator }

type Method =
    { Method: InfFunction
      MethodName: GenericName
      Modifiers: MethodModifiers
      SelfIdentifier: IdentifierStr option }

type PropAccessors =
    | AutoGet
    | AutoGetSet
    | Get of Statement list
    | GetSet of Statement list * InfParam * Statement list

type Property =
    { Accessors: PropAccessors
      PropName: SimpleName
      SelfIdentifier: IdentifierStr option
      Value: Expression option
      ValueType: TypeName option }

type AMethod =
    { Method: Function<unit, TypeName>
      MethodName: GenericName }

type AbstractPropAccessors =
    | AbstractGet
    | AbstractGetSet

type AProperty =
    { Accessors: AbstractPropAccessors
      PropName: SimpleName
      Purity: MutatorModf
      ValueType: TypeName }

type StaticFunction =
    { Function: InfFunction
      FunctionName: GenericName }

type AbstractMember =
    | AMethod of AMethod
    | AProperty of AProperty

type ConcreteMember =
    | Constructor of Ctor
    | Method of Method
    | Property of Property

type InstanceMember =
    | Abstract of AbstractMember
    | Concrete of ConcreteMember

type StaticMember =
    | Function of StaticFunction
    | Operator of Operator.Operator

type Member =
    | Instance of InstanceMember
    | Static of StaticMember

type MemberName =
    | IdentifierName of GenericName
    | OperatorName of Operator.OperatorStr

[<RequireQualifiedAccess>]
module Member =
    let inline private methodParams map def =
        ((^a) : (member Method : Function<_, _>) (def)).Parameters
        |> List.map (List.map map)
        |> Some
    let private emptyParams _ = None

    let withAccess<'Member> (acc: Access) (mdef: 'Member) = acc, mdef
    let defaultAccess<'Member> = withAccess<'Member> Access.Public

    let defaultCtor =
        { Body = List.empty
          Parameters = List.empty
          SelfIdentifier = None }

    let foldInstance amthd aprop ctor mthd prop mdef =
        match mdef with
        | Abstract adef ->
            match adef with
            | AMethod mdef -> amthd mdef
            | AProperty pdef -> aprop pdef
        | Concrete cdef ->
            match cdef with
            | Constructor c -> ctor c
            | Method mdef -> mthd mdef
            | Property pdef -> prop pdef

    let instanceParams =
        foldInstance
            (methodParams Param.asInferred)
            emptyParams
            (fun ctor -> Some [ ctor.Parameters ])
            (methodParams id)
            emptyParams

    let instanceName =
        let inline methodName def = Some ((^a) : (member MethodName : GenericName) (def))
        let inline propName def =
            ((^a) : (member PropName : SimpleName) (def))
            |> Name.asGeneric
            |> Some
        foldInstance
            methodName
            propName
            (fun _ -> None)
            methodName
            propName

    let instanceSig mdef =
        match mdef with
        | Abstract adef ->
            match adef with
            | AMethod mthd ->
                mthd.Method.Parameters
                |> Seq.map Param.toExpStr
                |> String.concat " "
                |> sprintf "%s%s" (string mthd.MethodName)
            | AProperty prop -> string prop.PropName
        | Concrete cdef ->
            match cdef with
            | Constructor ctor ->
                ctor.Parameters
                |> Param.toInfStr
                |> sprintf "new%s"
            | Method mthd ->
                mthd.Method.Parameters
                |> Seq.map Param.toInfStr
                |> String.concat " "
                |> sprintf "%s%s" (string mthd.MethodName)
            | Property prop -> string prop.PropName
