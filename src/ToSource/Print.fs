module Classier.NET.Compiler.ToSource.Print

open System
open Classier.NET.Compiler

let pclass (acc, gclass: GenClass) prnt =
    fun() ->
        seq {
            let minherit =
                match gclass.Syntax.Inheritance with
                | Grammar.CanInherit -> String.Empty
                | Grammar.MustInherit -> "abstract "
                | Grammar.Sealed -> "sealed "

            sprintf
                "%O %sclass %O%O "
                acc
                minherit
                gclass.ClassName
                () // TODO: Print generic arguments.

            match (gclass.Interfaces, gclass.SuperClass) with
            | (SortedSet.Empty, _)
            | (_, Some _) ->
                invalidOp "no impl"
            | _ -> String.Empty


        }
        |> Seq.iter prnt
