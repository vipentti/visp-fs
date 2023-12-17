// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.BuiltinMacros

open Visp.Compiler.Syntax
open Visp.Compiler.Transforms

let rec expand (expr: SynExpr) =
    let inner expr =
        match expr with
        | SynExpr.ThreadLast(items, range) -> SynExpr.ThreadLast(handleThreadables items, range)
        | _ -> expr

    Helpers.transform inner expr

and private handleThreadables (args: SynThreadable list) =
    match args with
    | arg :: rest ->
        let self =
            match arg with
            | SynThreadable.Expr(ex, range) -> (SynThreadable.Expr(expand ex, range))
            | SynThreadable.Index(ex, range) ->
                (SynThreadable.Expr(
                    SynExpr.LambdaDef(
                        SynLambda(
                            Syntax.mkInferredNamePat "a1" range,
                            [ SynExpr.DotIndex(Syntax.mkSynSymbolExpr "a1" range, ex, range) ],
                            range
                        )
                    ),
                    range
                ))
            | SynThreadable.Property(sym, range) ->
                (SynThreadable.Expr(
                    SynExpr.LambdaDef(
                        SynLambda(
                            Syntax.mkInferredNamePat "a1" range,
                            [ SynExpr.DotProperty(Syntax.mkSynSymbolExpr "a1" range, sym, range) ],
                            range
                        )
                    ),
                    range
                ))
            | SynThreadable.Method(sym, kind, range) ->
                (SynThreadable.Expr(
                    SynExpr.LambdaDef(
                        SynLambda(
                            Syntax.mkInferredNamePat "a1" range,
                            [ SynExpr.DotMethod(
                                  Syntax.mkSynSymbolExpr "a1" range,
                                  sym,
                                  [],
                                  kind,
                                  range
                              ) ],
                            range
                        )
                    ),
                    range
                ))

        self :: handleThreadables rest
    | [] -> []
