module FsGraphQL.UnitTests.Parser

open Expecto
open Assertions
open FsGraphQL.Ast
open FsGraphQL.Ast.Parser

let parseAndAssert expected query = parse query |> equals expected

[<Tests>]
let tests =
    testList "FsGraphQL.Ast.Parser tests" [
        test "Should be able to parse an empty document" {
            "" |> parseAndAssert { Definitions = [] }
        }
    ]