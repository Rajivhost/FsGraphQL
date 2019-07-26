#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.Testing.Expecto
nuget Fake.DotNet.Paket //"

#load ".fake/build.fsx/intellisense.fsx"

#if !FAKE
  #r "netstandard"
  #r "Facades/netstandard"
#endif

open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Restore" (fun _ ->
    !! "src/**/*.fsproj"
    ++ "tests/**/*.fsproj"
    |> Seq.iter (DotNet.restore id))

Target.create "Build" (fun _ ->
    !! "src/**/*.fsproj"
    ++ "tests/**/*.fsproj"
    |> Seq.iter (DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } })))

Target.create "Test" (fun _ ->
    !! "tests/**/bin/Release/*/*Tests.dll"
    ++ "tests/**/bin/Release/*/*Tests.exe"
    |> Expecto.run id)

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"