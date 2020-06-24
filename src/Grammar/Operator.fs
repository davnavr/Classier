module Classier.NET.Compiler.Grammar.Operator

open System.Text.RegularExpressions

type OperatorKind =
    | Infix
    | Prefix

type OperatorStr =
    | OperatorStr of string

    override this.ToString() =
        let (OperatorStr str) = this
        str

type Operator =
    { Body: PStatement list
      Kind: OperatorKind
      Operands: InfParam list
      ReturnType: TypeName option
      Symbol: OperatorStr }

let operatorChars = [ '!'; '%'; '&'; '*'; '+'; '-'; '/'; '<'; '='; '<'; '?'; '|'; '~' ]
