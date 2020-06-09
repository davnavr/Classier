module Classier.NET.Compiler.Grammar

open System
open System.Collections.Immutable

type Identifier = Identifier.Identifier<Generic.Generic>
type TypeName = TypeSystem.TypeName<Generic.Generic>

// TODO: Come up with better way to keep track of data that is not a flag enum.
[<Flags>]
type Flags =
    | None = 0u
    | Public = 1u
    | Internal = 2u
    | Protected = 3u
    | Private = 4u
    | VisibilityMask = 4u
    | Abstract = 8u
    /// Indicates that a class or local variable is mutable.
    | Mutable = 16u
    /// Indicates that a class can have a subclass.
    | Inheritable = 32u
    | Inline = 64u
    | Override = 128u
    /// Indicates that a method can be optionally overriden.
    | Virtual = 256u
    /// Indicates that a method cannot be overriden any further.
    | Sealed = 512u
    | MethodImplMask = 960u

[<Flags>]
type NumType =
    | Decimal = 1uy
    | Double = 2uy
    | Float = 3uy
    | Signed = 0uy
    | Unsigned = 4uy
    | Integer = 8uy
    | Long = 16uy

type NumLiteral =
    { Base: byte
      FracPart: char list
      IntPart: char list
      Type: NumType }

type Definition =
    { Flags: Flags
      Identifier: Identifier
      Position: FParsec.Position }

    static member empty =
        { Flags = Flags.None
          Identifier = { Name = String.Empty; Generics = [] }
          Position = FParsec.Position(String.Empty, 0L, 0L, 0L) }

    override this.ToString() = this.Identifier.ToString()

type Expression =
    | AnonFunc of Function
    | BoolLit of bool
    | CtorCall of
        {| Arguments: Expression list
           Type: TypeName |}
    | FuncCall of
        {| Arguments: Expression list list
           Target: Expression |}
    | FuncComp of Expression * Expression
    | IdentifierRef of Identifier
    | IfExpr of If
    | MatchExpr of Match
    | MemberAccess of Expression * Identifier
    | Nested of Expression
    | NumLit of NumLiteral
    | StrLit of string
    | ThrowExpr of Expression
    | TryExpr of Try
    | TupleLit of Expression list
    | UnitLit
    | VarAssignment of Assignment
and Assignment =
    { Target: Expression
      Value: Expression }
and If =
    { Condition: Expression
      Choice1: Statement list
      Choice2: Statement list }
and Match =
    { Against: Expression
      Cases: MatchCase list }
and MatchCase =
    { Body: Statement list
      Patterns: Pattern list }
and Pattern =
    | Constant of Expression
    | Default
    | TuplePattern of Param list
    | VarPattern of string * TypeName
and Statement =
    | Empty
    | IfStatement of If
    /// An expression whose result is evaluated then discarded.
    | IgnoredExpr of Expression
    /// Used in the body of classes and modules.
    | LocalMember of MemberDef
    | LocalVar of Variable
    | MatchStatement of Match
    | Return of Expression
    | Throw of Expression option
    | TryStatement of Try
    | While of Expression * Statement list
and Try =
    { TryBody: Statement list
      Handlers: MatchCase list
      Finally: Statement list }
and Variable =
    { Pattern: Pattern
      VarFlags: Flags
      Value: Expression option }
and Param =
    { Name: string
      Type: TypeName }

    static member toString (paramSet: Param list) =
        paramSet
        |> Seq.map string
        |> String.concat ", "
        |> sprintf "(%s)"

    static member toString (paramSets: Param list list) =
        paramSets
        |> Seq.map (Param.toString)
        |> String.concat " "

    override this.ToString () =
        match this.Type with
        | TypeName.Inferred -> String.Empty
        | _ -> sprintf " : %s" (string this.Type)
        |> sprintf "%s%s" this.Name
and Function =
    { Body: Statement list
      Parameters: Param list list
      ReturnType: TypeName }

    static member empty =
        { Body = List.empty
          Parameters = List.empty
          ReturnType = TypeName.Inferred }
and FunctionDef =
    { Function: Function
      FuncDef: Definition
      SelfIdentifier: string option }

    static member generatedDef =
        { Function = Function.empty
          FuncDef = Definition.empty
          SelfIdentifier = Some "&this" }

    static member placeholder def fparams =
        { Function = { Function.empty with Parameters = fparams }
          FuncDef = def
          SelfIdentifier = None }

and ClassHeader =
    { PrimaryCtor: Constructor
      SuperClass: Identifier list }
and TypeHeader =
    | Class of ClassHeader
    | Interface
    | Module
and TypeDef =
    { Header: TypeHeader
      InitBody: Statement list
      Interfaces: TypeName list
      Members: ImmutableSortedSet<MemberDef>
      TypeDef: Definition }
and Constructor =
    { BaseCall: ConstructorBase
      Body: Statement list
      CtorFlags: Flags
      Parameters: Param list }
and ConstructorBase =
    | SelfCall of Expression list
    | SuperCall of Expression list
and MemberDef =
    | Ctor of Constructor
    | Function of FunctionDef
    | Method of FunctionDef
    | NestedType of TypeDef

    static member definition mdef =
        match mdef with
        | Function fdef
        | Method fdef -> Some fdef.FuncDef
        | NestedType tdef -> Some tdef.TypeDef
        | _ -> None

    static member identifier mdef =
        MemberDef.definition mdef
        |> Option.map (fun def -> def.Identifier)

    /// Gets the parameters of the method, function, or constructor.
    static member paramSets mdef =
        match mdef with
        | Ctor ctor -> [ ctor.Parameters ]
        | Function fdef
        | Method fdef -> fdef.Function.Parameters
        | _ -> List.empty

    /// Gets the first parameter set of the method or function, or the parameters of the constructor.
    static member firstParams mdef =
        match mdef with
        | Ctor ctor -> Some ctor.Parameters
        | Function fdef
        | Method fdef -> List.tryHead fdef.Function.Parameters
        | _ -> None
        |> Option.defaultValue List.empty

type CompilationUnit =
    { Definitions: TypeDef list
      Namespace: string list
      Usings: Identifier list list }
