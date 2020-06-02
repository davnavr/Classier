namespace Classier.NET.Compiler.Grammar

[<System.Flags>]
type NumType =
    | Decimal = 0uy
    | Double = 1uy
    | Float = 2uy
    | Signed = 0uy
    | Unsigned = 4uy
    | Integer = 8uy
    | Long = 16uy

type NumLiteral =
    { Base: byte
      FracPart: char list
      IntPart: char list
      Type: NumType }
