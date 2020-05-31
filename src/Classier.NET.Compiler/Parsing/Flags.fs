namespace Classier.NET.Compiler

[<System.Flags>]
type Flags =
    | None = 0uy
    | Public = 1uy
    | Internal = 2uy
    | Protected = 3uy
    | Private = 4uy
    | VisibilityMask = 4uy
    | Abstract = 8uy
    /// Indicates that a class or local variable is mutable.
    | Mutable = 16uy
    /// Indicates that a class can have a subclass.
    | Inheritable = 32uy
    | Inline = 64uy
