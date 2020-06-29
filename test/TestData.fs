module Classier.NET.Compiler.TestData

open System.Reflection

type DataSource<'Data> = DataSource of (unit -> 'Data)

let map mapper (DataSource source) =
    DataSource (fun() -> source() |> mapper)

let embedded name =
    DataSource (fun() ->
        name
        |> sprintf "Classier.NET.Compiler.%s"
        |> Assembly.GetExecutingAssembly().GetManifestResourceStream
        |> Option.ofObj)
