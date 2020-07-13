namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type AnalyzerError =
    | BadEntryPointSignature of EntryPoint
    | DuplicateGlobalType of TypeDef * GlobalTypeSymbol
    | DuplicateClassMember of GenClass * TypeOrMember<Class, InstanceMember>
    | DuplicateInterfaceMember of GenInterface * TypeOrMember<Interface, AbstractMember>
    | DuplicateModuleMember of GenModule * TypeOrMember<TypeDef, StaticMember>
    | FeatureNotImplemented of feature: string

module AnalyzerError =
    let print =
        function
        | DuplicateClassMember(parent, dup) ->
            let mtext =
                match dup with
                | TypeOrMember.Type clss -> string clss.ClassName
                | TypeOrMember.Member cm ->
                    match Member.instanceName cm with
                    | Some name -> string name
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
        | err -> sprintf "%A" err
