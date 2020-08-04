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

let runProj proj args =
    let projFile =
        proj
        |> Path.getFullName
        |> FileInfo.ofPath
    if projFile.Exists then
        sprintf
            "--project %s --no-restore --configuration Release -- %s"
            projFile.FullName
            (String.concat " " args)
        |> DotNetCli.exec id "run"
        |> handleError
    else
        failwithf
            "Unable to run the project file %s as it does not exist\n"
            projFile.FullName

Target.create "Clean" (fun _ ->
    slnFile
    |> DotNetCli.exec id "clean"
    |> handleError
)

Target.create "Build Bootstrap" (fun _ ->
    DotNetCli.build id slnFile
)

Target.create "Lint Bootstrap" (fun _ ->
    slnFile
    |> sprintf "lint %s"
    |> DotNetCli.exec id "fsharplint"
    |> handleError
)

Target.create "Test Bootstrap" (fun _ ->
    runProj "./test/Classier.NET.Compiler.Tests.fsproj" []
)

Target.create "Build Samples" (fun _ ->
    runProj
        "./src/Classier.NET.Compiler.fsproj"
        [
            "\"./samples/SimpleHelloWorld/SimpleHello.txt\""
            "--output"
            "./samples/SimpleHelloWorld/Hello.output.cs"
        ]
    DotNetCli.build id "./samples/SimpleHelloWorld/SimpleHelloWorld.csproj"
)

Target.create "Publish" (fun _ ->
    Trace.trace "Publishing..."
)

"Clean" ==> "Build Bootstrap" ==> "Test Bootstrap" ==> "Build Samples" ==> "Publish"
"Clean" ==> "Build Bootstrap" ==> "Build Samples"

Target.runOrDefault "Publish"
