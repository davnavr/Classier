[<AutoOpen>]
module Classier.NET.Compiler.Grammar.Numeric

open System

[<RequireQualifiedAccess>]
type NumBase =
    | Binary
    | Decimal
    | Hexadecimal

type IntegralLit =
    { Base: NumBase
      Digits: string }

    override this.ToString() =
        let prefix =
            match this.Base with
            | NumBase.Binary -> "0b"
            | NumBase.Decimal -> String.Empty
            | NumBase.Hexadecimal -> "0x"
        sprintf "%s%s"
            prefix
            this.Digits

type FPointLit =
    { IntDigits: string
      FracDigits: string }

    override this.ToString() =
        sprintf "%s.%s"
            this.IntDigits
            this.FracDigits

[<RequireQualifiedAccess>]
type NumericLit =
    /// An integral literal of an unknown type.
    | Integral of IntegralLit
    /// An floating-point literal of an unknown type.
    | FPoint of FPointLit
    | Double of FPointLit
    | Float of FPointLit
    | Long of IntegralLit
