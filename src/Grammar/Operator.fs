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
    { Body: Statement list
      Kind: OperatorKind
      Operands: InfParam list
      Symbol: OperatorStr }

let operatorChars = [ '!'; '%'; '&'; '*'; '+'; '-'; '/'; '<'; '='; '<'; '?'; '|'; '~' ]
