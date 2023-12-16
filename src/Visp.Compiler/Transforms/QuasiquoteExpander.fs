// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler.Transforms

open Visp.Compiler.Syntax
open Visp.Compiler.Transforms
open Visp.Compiler.Text

module QuasiquoteExpander =

    let rec expand (expr: SynExpr) =
        let inner expr =
            match expr with
            | SynExpr.Quasiquote(_, expr, range) -> Syntax.mkValue (qqExpr expr range 1) range
            | _ -> expr

        Helpers.transform inner expr

    and expandBindings (lst: SynBinding list) =
        match lst with
        | (SynBinding(name, ex, range)) :: rest ->
            (SynBinding(name, (expand ex), range)) :: expandBindings rest
        | _ -> []

    and expandExprs (lst: SynExpr list) =
        match lst with
        | ex :: rest -> (expand ex) :: expandExprs rest
        | _ -> []

    and private expandSynOp (op: SynOp) =
        match op with
        | SynOp.Plus(args, range) -> SynOp.Plus(expandExprs args, range)
        | SynOp.Minus(args, range) -> SynOp.Minus(expandExprs args, range)
        | SynOp.Div(args, range) -> SynOp.Div(expandExprs args, range)
        | SynOp.Mult(args, range) -> SynOp.Mult(expandExprs args, range)

    and private expandSynLambda (SynLambda(args, body, range)) =
        SynLambda(args, expandExprs body, range)

    and private qqExpr (expr: SynQuasiquote) (range: range) (depth: int) =
        if depth < 0 then
            failwithf "unsupported depth"

        match expr with
        | SynQuasiquote.Unquote(expr, r) -> Syntax.mkValue expr r

        | SynQuasiquote.Collection(SynCollection(kind, exprs, range)) ->
            match kind with
            | CollectionKind.Paren -> qqExprs exprs range depth
            | _ -> failwithf "unsupported qq collection: %A %A" kind exprs

        | SynQuasiquote.Symbol sym -> SynExpr.Quote(false, (SynQuoted.Symbol sym), range)

        | SynQuasiquote.Const(c, r) -> SynExpr.Quote(false, SynQuoted.Const(c, r), r)

        | SynQuasiquote.Keyword k -> SynExpr.Keyword k


        | _ -> failwithf "unsupported quasiquote %O at %O" expr range
    // | _ -> SynExpr.Const (SynConst.Nil, range)

    and private qqExprs (exprs: SynQuasiquote list) (range: range) (depth: int) =
        //let concat = Syntax.mkSynSymbol "concat" range
        //let cons = Syntax.mkSynSymbol "cons" range

        let mutable result = Syntax.mkEmptyList range

        let reversed = List.rev exprs

        for expr in reversed do
            match expr with
            | SynQuasiquote.SpliceUnquote(expr, r) ->
                result <- Syntax.mkConcat (Syntax.mkUnwrapList expr r) result range
            | _ -> result <- Syntax.mkCons (qqExpr expr range depth) result range

        result

    and private toSynExpr (expr: SynQuasiquote) (range: range) =
        match expr with
        | SynQuasiquote.Const(c, r) -> SynExpr.Const(c, r)

        | SynQuasiquote.Symbol sym -> SynExpr.Symbol sym

        | SynQuasiquote.Keyword sym -> SynExpr.Keyword sym

        // | SynQuasiquote.Vector(exprs, range) -> SynExpr.Vector(toSynExprs exprs range, range)

        | _ -> failwithf "unsupported expr %O at %O" expr range
    //| _ -> SynExpr.Const (SynConst.Nil, range)

    and private toSynExprs (exprs: SynQuasiquote list) (range: range) =
        match exprs with
        | ex :: rest -> (toSynExpr ex range) :: (toSynExprs rest range)
        | _ -> []

    and private toQuoted (ex: SynQuasiquote) (range: range) =
        match ex with
        | SynQuasiquote.Collection(SynCollection(kind, exprs, range)) ->
            SynQuoted.Collection(SynCollection(kind, (toQuotedList exprs range), range))
        | _ -> failwithf "unsupported expr %O at %O" ex range

    and private toQuotedList (ex: SynQuasiquote list) (range: range) =
        match ex with
        | a :: rest -> (toQuoted a range) :: (toQuotedList rest range)
        | _ -> []
//and private toSynExpr (ex: SynQuasiquote) (range: range) =
//    match ex with
//    | SynQuasiquote.List (exprs, _) ->
//        SynExpr.Quote (SynQuoted.List ())


//    | _ -> SynExpr.Const (SynConst.Nil, range)

//and private toSynExprs (ex: SynQuasiquote list) (range: range) =
//    match ex with
//    | arg :: rest ->
//        (toSynExpr arg range) :: (toSynExprs rest range)
//    | _ -> []
