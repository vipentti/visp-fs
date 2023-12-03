// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.BuiltinMacros

open Visp.Compiler.Syntax
open Visp.Compiler.Transforms

let rec expand (expr: SynExpr) =
    let inner expr =
        match expr with
        | SynExpr.FunctionCall(Patterns.SymbolWith "and", args, range) -> handleAnd args range
        | SynExpr.FunctionCall(Patterns.SymbolWith "or", args, range) -> handleOr args range
        | SynExpr.FunctionCall(Patterns.SymbolWith "cond", args, range) -> handleCond args range
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
                            [ Syntax.mkInferredArg "a1" range ],
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
                            [ Syntax.mkInferredArg "a1" range ],
                            [ SynExpr.DotProperty(Syntax.mkSynSymbolExpr "a1" range, sym, range) ],
                            range
                        )
                    ),
                    range
                ))
            | SynThreadable.Method(sym, range) ->
                (SynThreadable.Expr(
                    SynExpr.LambdaDef(
                        SynLambda(
                            [ Syntax.mkInferredArg "a1" range ],
                            [ SynExpr.DotMethod(Syntax.mkSynSymbolExpr "a1" range, sym, [], range) ],
                            range
                        )
                    ),
                    range
                ))

        self :: handleThreadables rest
    | [] -> []

and private handleCond (args: SynExpr list) range =
    match args with
    // TODO: handle this
    | [] -> // (SynExpr.Const(SynConst.Bool false, range))
        Syntax.mkFunctionCall
            (Syntax.mkSynSymbolExpr "failwith" range)
            [ Syntax.mkSynString "Unbalanced cond" range ]
            range
    | var :: body :: exprs ->
        let rest = handleCond exprs range
        SynExpr.If(var, body, Some rest, range)
    | _ -> failwith "unbalanced cond"

and private handleAnd (args: SynExpr list) range =
    match args with
    | [] -> SynExpr.Const(SynConst.Bool true, range)
    | [ one ] -> one
    | cond :: exprs ->
        let rest = handleAnd exprs range
        SynExpr.If(cond, rest, Some(SynExpr.Const(SynConst.Bool false, range)), range)

and private handleOr (args: SynExpr list) range =
    match args with
    | [] -> SynExpr.Const(SynConst.Bool false, range)
    | [ one ] -> one
    | cond :: exprs ->
        let orTemp = Syntax.mkSynSymbol "ortemp" range
        let rest = handleOr exprs range

        SynExpr.LetStar(
            [ SynBinding(SynName.Inferred(orTemp, range), cond, range) ],
            [ SynExpr.If(SynExpr.Symbol orTemp, SynExpr.Symbol orTemp, Some rest, range) ],
            range
        )

and private threadFirst (args: SynExpr list) range =
    match args with
    | [] -> failwith "missing args"
    | [ one ] -> one
    | var :: rest ->
        let rec loop var (forms: SynExpr list) =
            match forms with
            | form :: rest ->
                match form with
                | SynExpr.FunctionCall(fn, args, range) ->
                    match args with
                    | fst :: args -> loop (SynExpr.FunctionCall(fn, fst :: var :: args, range)) rest
                    | [] -> loop (SynExpr.FunctionCall(fn, [ var ], range)) rest
                | SynExpr.LambdaDef def as it ->
                    loop (SynExpr.FunctionCall(it, [ var ], def.Range)) rest
                | _ -> failwithf "not supported: %O" form
            | _ -> var

        loop var rest
//Syntax.mkEmptyList range
