// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.Common

open Visp.Compiler.Syntax
open System.Collections.Generic

let transformLambdaShortHands (expr: SynExpr) =
    let transform expr =
        match expr with
        | SynExpr.LambdaShort(expr, range) ->
            let mutable index = 0
            let dict = new Dictionary<string, string>()
            let parameters = new ResizeArray<_>()

            Helpers.transform
                (fun it ->
                    match it with
                    | SynExpr.Symbol(SynSymbol(id)) ->
                        if id.idText.StartsWith('%') && not (dict.ContainsKey(id.idText)) then
                            let name = "arg" + id.idText.TrimStart('%') + (index.ToString())
                            dict.[id.idText] <- name
                            index <- index + 1

                            parameters.Add(
                                SynArg.InferredArg(Syntax.mkSynSymbol name id.idRange, id.idRange)
                            )

                        ()
                    | _ -> ()

                    it)
                expr
            |> ignore

            let body =
                Helpers.transform
                    (fun helpers ->
                        match helpers with
                        | SynExpr.Symbol(SynSymbol(id)) as orig ->
                            match dict.TryGetValue(id.idText) with
                            | false, _ -> orig
                            | true, newName -> (Syntax.mkSynSymbolExpr newName id.idRange)
                        | _ -> helpers)
                    expr

            SynExpr.LambdaDef(SynLambda(List.ofSeq (parameters), [ body ], range))

        | _ -> expr

    Helpers.transform transform expr
