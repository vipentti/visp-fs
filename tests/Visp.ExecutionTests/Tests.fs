module Tests

open Xunit

[<Fact>]
let ``Basic execution`` () =
    TestUtils.runTest "tests/examples/example-0.visp"
