module FsGraphQL.UnitTests.Assertions

open Expecto

let equals actual expected = 
    Expect.equal actual expected (sprintf "Expected %A, but got %A." expected actual)