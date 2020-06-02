namespace Classier.NET.Compiler.Parsing

[<System.Flags>]
type Flags =
    | None = 0u
    | Public = 1u
    | Internal = 2u
    | Protected = 3u
    | Private = 4u
    | VisibilityMask = 4u
    | Abstract = 8u
    /// Indicates that a class or local variable is mutable.
    | Mutable = 16u
    /// Indicates that a class can have a subclass.
    | Inheritable = 32u
    | Inline = 64u
    | Override = 128u
    /// Indicates that a method can be optionally overriden.
    | Virtual = 256u
    /// Indicates that a method cannot be overriden any further.
    | Sealed = 512u
    | MethodImplMask = 960u
