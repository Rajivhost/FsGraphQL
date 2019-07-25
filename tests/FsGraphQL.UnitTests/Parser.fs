module FsGraphQL.UnitTests.Parser

open Expecto
open Assertions
open FsGraphQL.Ast
open FsGraphQL.Ast.Parser

let parseAndAssert expected query = parse query |> equals expected

let queryShorthand selectionSet = 
    selectionSet
    |> QueryShorthand
    |> OperationDefinition 
    |> ExecutableDefinition

let operation name variables directives selectionSet =
    { OperationType = Query
      Name = name
      VariableDefinitions = variables
      Directives = directives
      SelectionSet = selectionSet }

let operationWithNameAndSelectionSet name selectionSet =
    selectionSet
    |> operation name [] []
    |> Operation
    |> OperationDefinition
    |> ExecutableDefinition

let operationWithSelectionSet = operationWithNameAndSelectionSet None

let field name alias directives args selectionSet =
    { Name = name
      Alias = alias
      Directives = directives
      Arguments = args
      SelectionSet = selectionSet }
    |> Field

let fieldWithName name = field name None [] [] []

let documentWithDefinitions definitions = { Definitions = definitions }
let emptyDocument = documentWithDefinitions []
let documentWithDefinition definition = documentWithDefinitions [ definition ]
let shortHandSelectionSet selectionSet = selectionSet |> queryShorthand |> documentWithDefinition
let shortHandSelection selection = shortHandSelectionSet [ selection ]
let unamedQueryWithSelectionSet selectionSet = selectionSet |> operationWithSelectionSet |> documentWithDefinition
let unamedQueryWithSelection selection = unamedQueryWithSelectionSet [ selection ]
let queryWithSelectionSet name selectionSet = operationWithNameAndSelectionSet name selectionSet |> documentWithDefinition
let queryWithSelection name selection = queryWithSelectionSet (Some name) [ selection ]

[<Tests>]
let tests =
    testList "FsGraphQL.Ast.Parser tests" [

        test "Should be able to parse an empty document" {
            parseAndAssert emptyDocument ""
        }

        test "Should be able to parse a document with white spaces, line terminators and commas" {
            [| '\u0009'; '\u000B'; '\u000C'; '\u0020'; '\u00A0'; '\u000A'; '\u000D'; '\u2028'; '\u2029'; ',' |]
            |> System.String
            |> parseAndAssert emptyDocument
        }

        test "Should be able to parse a document with comments" {
            "# This is a comment, should be ignored"
            |> parseAndAssert emptyDocument
        }

        test "Should be able to parse a query with a single field in shorthand format" {
            let expected = 
                fieldWithName "id"
                |> shortHandSelection
            parseAndAssert expected "{id}"
        }

        test "Should be able to parse an unamed query with a single field" {
            let expected = 
                fieldWithName "id"
                |> unamedQueryWithSelection
            parseAndAssert expected "query {id}"
        }

        test "Should be able to parse a named query with a single field" {
            let expected = 
                fieldWithName "id"
                |> queryWithSelection "MyQuery"
            parseAndAssert expected "query MyQuery {id}"
        }

        test "Should be able to ignore whitespaces and line terminators in a simple query" {
            let expected = 
                fieldWithName "id"
                |> queryWithSelection "MyQuery"
            parseAndAssert expected """query MyQuery {
                  id
                }"""
        }
    ]