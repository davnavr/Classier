namespace Classier.NET.Compiler.Grammar

open Classier.NET.Compiler.Identifier

type AbstractModf =
    | Abstract
    | Concrete

type MutatorModf =
    | IsMutator
    | IsPure

type CtorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list

type Ctor =
    { BaseCall: CtorBase
      Body: Statement list
      Parameters: InfParam list
      SelfIdentifier: IdentifierStr option }

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

type PropAccessors =
    | AutoGet
    | AutoGetSet
    | Get of Statement list
    | GetSet of Statement list * InfParam * Statement list

type AbstractPropAccessors =
    | AbstractGet
    | AbstractSet

type AbstractMember =
    | AMethod of
        {| Method: Function<unit, TypeName>
           MethodName: Name
           Modifiers: AbstractModf * MutatorModf |}
    | AProperty of
        {| Accessors: AbstractPropAccessors
           PropName: Name
           Purity: AbstractModf
           ValueType: TypeName |}

type ConcreteMember =
    | Constructor of Ctor
    | Method of
        {| Method: InfFunction
           MethodName: Name
           Modifiers: MethodModifiers
           SelfIdentifier: IdentifierStr option |}
    | Property of
        {| Accessors: PropAccessors
           PropName: Name
           SelfIdentifier: IdentifierStr option
           Value: Expression option
           ValueType: TypeName option |}

type InstanceMember =
    | Abstract of AbstractMember
    | Concrete of ConcreteMember

type StaticMember =
    | Function of
        {| Function: InfFunction
           FunctionName: Name |}
    | Operator of Operator.Operator

type Member =
    | Instance of InstanceMember
    | Static of StaticMember

module Member =
    let withAccess (acc: Access) (mdef: ConcreteMember) = acc, mdef
    let defaultAccess = withAccess Access.Public

    let name mdef =
        match mdef with
        | Constructor _ -> None
        | Function fdef -> Some fdef.FunctionName
        | Method mdef -> Some mdef.MethodName
        | Property pdef -> Some pdef.PropName

    let identifier mdef =
        Option.map
            (fun def -> def.Identifier)
            (name mdef)

    /// Gets the parameters of the method, function, or constructor.
    let paramSets mdef =
        match mdef with
        | Constructor ctor -> [ ctor.Parameters ]
        | Function fdef -> fdef.Function.Parameters
        | Method mdef -> mdef.Method.Parameters
        | _ -> List.empty
    /// Gets the first parameter set of the method or function, or the parameters of the constructor.
    let firstParams mdef =
        mdef
        |> paramSets
        |> List.tryHead
        |> Option.defaultValue List.empty

    let compare m1 m2 = // TODO: Use Member instead of ConcreteMember and add checks for abstract methods.
        let paramCompare =
            List.compareWith
                (fun p1 p2 ->
                    match (p1.Type, p2.Type) with
                    | (None, _)
                    | (_, None) -> 0
                    | (Some t1, Some t2) -> compare t1 t2)
                (firstParams m1)
                (firstParams m2)
        match paramCompare with
        | 0 ->
            match (m1, m2) with
            | (Function _, Method _) -> -1
            | (Method _, Function _) -> 1
            | (_, _) ->
                compare
                    (identifier m1)
                    (identifier m2)
        | _ -> paramCompare

    // TODO: Remove as many placeholder functions as possible.
    let placeholderCtor cparams =
        { BaseCall = SuperCall List.empty
          Body = List.empty
          Parameters = cparams
          SelfIdentifier = None }
    
    let emptyCtor = placeholderCtor List.empty
    
    let internal placeholderMethod name selfid mparams =
        Method
            {| Method =
                { Body = List.empty
                  Parameters = mparams
                  ReturnType = None }
               MethodName = name
               Modifiers = MethodModifiers.Default
               SelfIdentifier = selfid |}
