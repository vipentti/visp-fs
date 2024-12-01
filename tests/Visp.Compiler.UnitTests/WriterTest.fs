// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md


module WriterTests

open Xunit
open VerifyXunit
open Visp.Compiler.Core
open System.IO

let getExampleFilePath name =
    let src_dir = __SOURCE_DIRECTORY__

    Path.Combine(src_dir, "..", "..", "visp", name) |> Path.GetFullPath

[<Fact>]
let ``Can output basic program`` () =
    let program =
        """
;; create a function named hello
;; which takes one argument with the name 'name' and type 'string'
(fn hello ((name : string))
    ;; call function with the name printfn with the provided arguments
    (printfn "hello %s" name))

(hello "world")
"""

    let parsed = CoreParser.parseString program "test"

    use stream = new StringWriter()

    CoreParser.writeToStreamNew parsed stream "test"

    Verifier.Verify(stream.ToString()).UseDirectory("snapshots").ToTask()
    |> Async.AwaitTask


[<Fact>]
let ``Can recover from basic parsing errors`` () =
    let program =
        """
(hello "world"
"""

    let parsed = CoreParser.parseString program "test"

    use stream = new StringWriter()

    CoreParser.writeToStreamNew parsed stream "test"

[<Fact(Skip = "Recovery")>]
let ``Can recover from parsing errors`` () =
    let program =
        """
;; create a function named hello
;; which takes one argument with the name 'name' and type 'string'
(fn hello ((name : string))
    ;; call function with the name printfn with the provided arguments
    (printfn "hello %s" name

(hello "world")
"""

    let parsed = CoreParser.parseString program "test"

    use stream = new StringWriter()

    CoreParser.writeToStreamNew parsed stream "test"

[<Fact>]
let ``New writer can output basic program`` () =
    let program =
        """
;; create a function named hello
;; which takes one argument with the name 'name' and type 'string'
(fn hello ((name : string))
    ;; call function with the name printfn with the provided arguments
    (sprintf "hello %s" name))

(hello "world")
"""

    let parsed = CoreParser.parseString program "test"

    use stream = new StringWriter()

    CoreParser.writeToStreamNew parsed stream "test"

    Verifier.Verify(stream.ToString()).UseDirectory("snapshots").ToTask()
    |> Async.AwaitTask
