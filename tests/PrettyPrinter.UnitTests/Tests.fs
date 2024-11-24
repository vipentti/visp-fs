module Tests

open Xunit
open FsUnit.Xunit
open PrettyPrinter.Print
open VerifyXunit

[<Fact>]
let ``dummy test`` () = (1 + 2) |> should equal 3

[<Fact>]
let ``can pretty print a document`` () =
    (*
(syntax-macro matchfnexample
  [(_ sym m ...)
    (fn sym (arg)
      (match arg
        m
        ...
      ))
  ])
    *)
    let doc =
        parens (
            (hsep [ text "syntax-macro"; text "matchfnexample" ])
            <~> (indent
                2
                (brackets (
                    vsep
                        [ (parens (hsep [ text "_"; text "sym"; text "m"; text "..." ]))

                          (indent
                              2
                              (parens (
                                  vsep
                                      [ hsep [ text "fn"; text "sym"; parens (text "arg") ]
                                        indent
                                            2
                                            (parens (
                                                vsep
                                                    [ (hsep [ text "match"; text "arg" ])
                                                      indent 2 <| vsep [ text "m"; text "..." ] ]
                                            )) ]
                              ))) ]
                )))
        )

    let simple = renderPrettyDefault doc

    let str = displayString simple

    Verifier.Verify(str).UseDirectory("snapshots").DisableDiff().ToTask()
    |> Async.AwaitTask
