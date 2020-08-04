[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.Extern.EType

let gname =
    function
    | EGlobalClass clss -> clss.ClassName
    | EGlobalInterface intf -> intf.InterfaceName
    | EGlobalModule mdle -> mdle.ModuleName
