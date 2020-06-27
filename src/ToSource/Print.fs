module Classier.NET.Compiler.ToSource.Print

open System
open Classier.NET.Compiler

let pclass gclass prnt =
    fun() ->
        seq {
            let minherit =
                match gclass.Syntax.Inheritance with
                | Grammar.CanInherit -> String.Empty
                | Grammar.MustInherit -> "abstract "
                | Grammar.Sealed -> "sealed "

            sprintf
                "%O %sclass %O%O "
                gclass.Access
                minherit
                gclass.Name.Name
                () // TODO: Print generic arguments.

            match (gclass.Interfaces, gclass.SuperClass) with
            | (SortedSet.Empty, _)
            | (_, Some _) ->
                invalidOp "no impl"
            | _ -> String.Empty


        }
        |> Seq.iter prnt
