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

open System.IO
open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.IO.FileSystemOperators

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Restore" (fun _ ->
    !! "src/**/*.fsproj"
    |> Seq.iter (DotNet.restore id))

Target.create "Build" (fun _ ->
    !! "src/**/*.fsproj"
    |> Seq.iter (DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } })))

Target.create "Test" (fun _ ->
    !! "tests/**/bin/Release/*/*Tests.dll"
    |> Expecto.run id)

let project = "FsGraphQL"
// let summary = "GraphQL language implementation for F#"
// let release = ReleaseNotes.load "RELEASE_NOTES.md"

// Target.create "AssemblyInfo" (fun _ ->
//     let getAssemblyInfoAttributes projectName =
//         [ AssemblyInfo.Title projectName
//           AssemblyInfo.Product project
//           AssemblyInfo.Description summary
//           AssemblyInfo.Version release.AssemblyVersion
//           AssemblyInfo.FileVersion release.AssemblyVersion ]
//     let getProjectDetails projectPath =
//         let projectName = Path.GetFileNameWithoutExtension(projectPath)
//         projectPath,
//         projectName,
//         Path.GetDirectoryName(projectPath),
//         (getAssemblyInfoAttributes projectName)
//     let internalsVisibility (projectFileName : string) =
//         match projectFileName with
//         | "FsGraphQL.Ast.fsproj" -> [ AssemblyInfo.InternalsVisibleTo "FsGraphQL.UnitTests" ]
//         | _ -> []
//     !! "src/**/*.fsproj"
//     |> Seq.map getProjectDetails
//     |> Seq.iter (fun (fileName, _, folderName, attributes) ->
//             AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") (attributes @ internalsVisibility fileName)))

// let pack id =
//     Shell.cleanDir <| sprintf "nuget/%s.%s" project id
//     Paket.pack (fun p ->
//         { p with
//             Version = release.NugetVersion
//             OutputPath = sprintf "nuget/%s.%s" project id
//             TemplateFile = sprintf "src/%s.%s/%s.%s.fsproj.paket.template" project id project id
//             IncludeReferencedProjects = true
//             MinimumFromLockFile = true
//             ReleaseNotes = release.Notes |> List.reduce (+) })

// let publish id =
//     pack id
//     Paket.push (fun options ->
//         { options with
//             WorkingDir = sprintf "nuget/%s.%s" project id
//             PublishUrl = "https://www.nuget.org/api/v2/package" })

// Target.create "PackAst" (fun _ -> pack "Ast")

// Target.create "PublishAst" (fun _ -> publish "Ast")

Target.create "All" ignore

"Clean"
  ==> "Restore"
  //==> "AssemblyInfo"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"