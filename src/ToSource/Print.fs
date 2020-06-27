module Classier.NET.Compiler.ToSource.Print

let pclass gclass prnt =
    sprintf
        "%O /*TODO: What about the modifiers?*/ class"
        gclass.Access
    invalidOp "bad"
