#if FAKE_DEPENDENCIES
#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
//"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO

// Workaround because intellisense doesn't want to cooperate
module DotNetCli = Fake.DotNet.DotNet

let slnFile = "Classier.NET.Compiler.sln"

Target.create "Clean" (fun _ ->
    slnFile
    |> DotNetCli.exec id "clean"
    |> ignore
)

Target.create "Build" (fun _ ->
    DotNetCli.build id slnFile
)

Target.create "Lint" (fun _ ->
    slnFile
    |> sprintf "lint %s"
    |> DotNetCli.exec id "fsharplint"
    |> ignore
)

Target.create "Test" (fun _ ->
    DotNetCli.test id slnFile
)

Target.create "Publish" (fun _ ->
    ()
)

"Clean" ==>
"Build" ==>
"Lint" ==>
"Test" ==>
"Publish"

// start build
Target.runOrDefault "Publish"
