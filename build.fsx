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

Target.create "Clean" (fun _ ->
    DotNetCli.exec id "clean" "" |> ignore
)

Target.create "Build" (fun _ ->
    DotNetCli.build id "Classier.NET.Compiler.sln"
)

Target.create "Lint" (fun _ ->
    DotNetCli.exec id "fsharplint" "lint Classier.NET.Compiler.sln"
    |> ignore
)

Target.create "Test" (fun _ ->
    let testsPath = Path.getFullName "test/"

    [ "bin"; "obj" ]
    |> Seq.map (Path.combine testsPath)
    |> Shell.cleanDirs

    testsPath
    |> DirectoryInfo.ofPath
    |> DirectoryInfo.getMatchingFiles "*.fs"
    |> Seq.iter
        (fun testFile ->
            let testName = Path.GetFileNameWithoutExtension testFile.Name

            Trace.logfn "Running tests for %s..." testName

            DotNetCli.build
                (fun options ->
                    { options with MSBuildParams = { options.MSBuildParams with Properties = [ "TestFile", testName ] } })
                (Path.getFullName "test/Classier.NET.Compiler.Tests.fsproj")

            sprintf
                "bin/Release/netcoreapp3.1/%s.dll"
                testName
            |> Path.combine testsPath
            |> DotNetCli.exec
                id
                "exec"
            |> ignore)
)

Target.create "Publish" (fun _ ->
    ()
)

"Clean" ==> "Build" ==> "Test" ==> "Publish"

// start build
Target.runOrDefault "Publish"
