namespace Classier.NET.Compiler.SemAnalysis

open System.Collections.Immutable
open Classier.NET.Compiler
open Classier.NET.Compiler.Grammar
open Classier.NET.Compiler.IR

type MemberAnalyzer<'Errors> =
    { Errors: 'Errors
      Type: GenType }

module MemberAnalyzer =
    let init tdef =
        let rec classm (clss: GenClass) =
            match clss.Syntax.Members with
            | [] -> clss
            | (acc, msyntax) :: mtail ->
                let nmember =
                    match msyntax with
                    | TypeOrMember.Type nested ->
                        nested
                        |> GenType.gclass
                            ImmutableSortedSet.Empty // TODO: Create way to handle creation of interface sets.
                            MemberSet.classSet
                        |> classm
                        |> TypeOrMember.Type
                    | TypeOrMember.Member mdef ->
                        
                        invalidOp "bad"
                SortedSet.tryAdd // TODO: How to handle duplicate member?
                { clss with
                    Members = clss.Members.Add(acc, nmember)
                    Syntax = { clss.Syntax with Members = mtail } }
                |> classm
        match tdef with
        | GenClass clss ->
            GenClass { clss with Members = clss.Members } // TODO: How will the primary constructor and class body be handled?
        | GenInterface intf -> invalidOp "bad"
        | GenModule mdle -> invalidOp "bad"
