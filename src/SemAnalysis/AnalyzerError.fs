namespace Classier.NET.Compiler.SemAnalysis

open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type AnalyzerError =
    | DuplicateGlobalType of TypeDef * GlobalTypeSymbol
    | DuplicateClassMember of GenClass * TypeOrMember<Class, InstanceMember>
    | DuplicateInterfaceMember of GenInterface * TypeOrMember<Interface, AbstractMember>
    | DuplicateModuleMember of GenModule * TypeOrMember<TypeDef, StaticMember>

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
        | err -> sprintf "%A" err
