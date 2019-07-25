module FsGraphQL.UnitTests.Program

open Expecto
open Expecto.Logging

[<EntryPoint>]
let main args =
    let config = { defaultConfig with verbosity = Verbose }
    runTestsInAssembly config args
