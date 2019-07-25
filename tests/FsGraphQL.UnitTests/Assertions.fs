module FsGraphQL.UnitTests.Assertions

open Expecto

let equals actual expected = Expect.equal actual expected "Unexpected value"