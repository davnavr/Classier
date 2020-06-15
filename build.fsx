#if FAKE_DEPENDENCIES
#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
//"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO

// Workaround because intellisense doesn't want to cooperate
module DotNetCli = Fake.DotNet.DotNet

let slnFile = "Classier.NET.Compiler.sln"

let handleError (result: ProcessResult) =
    match result.ExitCode with
    | 0 -> ()
    | code -> failwithf "Process returned with an exit code of %i\n" code

Target.create "Clean" (fun _ ->
    slnFile
    |> DotNetCli.exec id "clean"
    |> handleError
)

Target.create "Build" (fun _ ->
    DotNetCli.build id slnFile
)

Target.create "Lint" (fun _ ->
    slnFile
    |> sprintf "lint %s"
    |> DotNetCli.exec id "fsharplint"
    |> handleError
)

Target.create "Test" (fun _ ->
    "./test/Classier.NET.Compiler.Tests.fsproj"
    |> Path.getFullName
    |> sprintf "--project %s --no-build"
    |> DotNetCli.exec id "run"
    |> handleError
)

Target.create "Publish" (fun _ ->
    ()
)

"Clean" ==>
"Build" ==>
"Lint" ==>
"Test" ==>
"Publish"

Target.runOrDefault "Publish"
