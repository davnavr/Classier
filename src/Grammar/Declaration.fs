[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Classier.NET.Compiler.Grammar.Declaration

let name =
    function
    | Declaration.Defined(_, ddecl) ->
        match ddecl with
        | Class clss -> clss.ClassName
        | Interface intf -> intf.InterfaceName
        | Module mdle -> Name.asGeneric mdle.ModuleName
    | Declaration.Extern edecl ->
        match edecl with
        | ExternClass clss -> clss.ClassName
        | ExternInterface intf -> intf.InterfaceName
        | ExternModule mdle -> Name.asGeneric mdle.ModuleName
