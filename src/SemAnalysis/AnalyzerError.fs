namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler

open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type AnalyzerError =
    | BadEntryPointSignature of EntryPoint
    | BadUseStatement of FParsec.Position * FullIdentifier<TypeName>
    | DuplicateGlobalSymbol of GenGlobalType * existing: Globals.Symbol
    | DuplicateClassMember of GenClass * TypeOrMember<Class, ClassMember>
    | DuplicateInterfaceMember of GenInterface * TypeOrMember<Interface, AbstractMember>
    | DuplicateModuleMember of GenModule * TypeOrMember<Declaration, ModuleMember>
    | FeatureNotImplemented of feature: string
    | InternalAnalyzerError of msg: string
    | LocalsTableError of LocalsTable.Error

module AnalyzerError =
    let print =
        function
        | DuplicateClassMember(parent, dup) ->
            let mtext =
                match dup with
                | TypeOrMember.Type clss -> string clss.ClassName
                | TypeOrMember.Member cm ->
                    match Member.instanceName cm with
                    | Some name -> string name // TODO: Maybe include signature of methods?
                    | _ -> "a constructor"
            sprintf
                "The class '%O' contains duplicate definitions for %s"
                parent.ClassName
                mtext
        | BadEntryPointSignature epoint ->
            sprintf
                "The signature of the entry point at %O is invalid"
                epoint.Origin
        | FeatureNotImplemented feature ->
            sprintf "%s is not yet implemented" feature
        | InternalAnalyzerError msg ->
            sprintf "Internal analyzer error: %s" msg
        | err -> sprintf "%A" err
