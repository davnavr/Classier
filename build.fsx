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

// Workaround because intellisense doesn't want to cooperate
module DotNetCli = Fake.DotNet.DotNet

Target.create "Clean" (fun _ ->
    DotNetCli.exec id "clean" "" |> ignore
)

Target.create "Build" (fun _ ->
    DotNetCli.build id "Classier.NET.Compiler.sln"
)

Target.create "Test" (fun _ ->
    DotNetCli.test id ""
)

Target.create "Publish" (fun _ ->
    ()
)

//"Clean" ==> "Build" ==> "Test" ==> "Publish"
"Test" ==> "Publish"

// start build
Target.runOrDefault "Publish"
