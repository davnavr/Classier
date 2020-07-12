﻿module Classier.NET.Compiler.IR.GenMember

open System.Collections.Immutable
open Classier.NET.Compiler.TypeSystem
open Classier.NET.Compiler.Grammar.Ast

let classm =
    function
    | Abstract mabs ->
        invalidOp "bad"
    | Concrete mcon ->
        match mcon with
        | Method mthd ->
            invalidOp "bad"
        | Constructor ctor ->
            { Parameters =
                ctor.Parameters
                |> Seq.map (fun param ->
                    { Name = param.Name
                      Syntax = param
                      Type = Primitive PrimitiveType.Unit })
                |> ImmutableList.CreateRange
              SelfCall = PrimaryCtorCall ImmutableList.Empty
              Syntax = ctor }
            |> ClassCtor