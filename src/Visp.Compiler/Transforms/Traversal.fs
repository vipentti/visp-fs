// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.Traversal

open System.Collections.Generic
open Visp.Compiler.Syntax

[<RequireQualifiedAccess>]
type WalkEvent<'a> =
    | Enter of 'a
    | Leave of 'a

let inline private enter a = WalkEvent.Enter a
let inline private leave a = WalkEvent.Enter a

let inline private MakeRefComparer<'T when 'T: not struct> () =
    { new IEqualityComparer<'T> with
        member _.GetHashCode(x) = System.HashCode.Combine(x)
        member _.Equals(x, y) = LanguagePrimitives.PhysicalEquality x y }

type private TraversalTree<'T when 'T: not struct>() =
    let cmp = MakeRefComparer<'T>()
    let parents = Dictionary<'T, 'T>(cmp)
    let children = Dictionary<'T, ResizeArray<'T>>(cmp)
    let siblings = Dictionary<'T, Queue<'T>>(cmp)
    let items = ResizeArray<'T>()

    member d.Add child parent =
        d.AddChild parent child
        children[child] <- ResizeArray()
        items.Add(child)

    member _.Parent t =
        match parents.TryGetValue t with
        | false, _ -> None
        | true, it -> Some(it)

    member _.AddChild parent child =
        let childs =
            match children.TryGetValue(parent) with
            | false, _ ->
                let it = ResizeArray()
                children[parent] <- it
                it
            | true, it -> it

        childs.Add(child)

let depthFirstMacroBodyPred (pred: SynMacroBody -> bool) (bod: SynMacroBody) =
    let rec main_loop (pred: SynMacroBody -> bool) (bod: SynMacroBody) =
        let loop = main_loop pred

        seq {
            yield bod

            if pred bod then
                match bod with
                | SynMacroBody.Call(SynMacroCall(_, it, _))
                | SynMacroBody.List(_, it, _) ->
                    for e in it do
                        yield! loop e

                | SynMacroBody.Discard _
                | SynMacroBody.Ellipsis _
                | SynMacroBody.Symbol _
                | SynMacroBody.Keyword _
                | SynMacroBody.Trivia _
                | SynMacroBody.Const _ -> ()
        }

    main_loop pred bod

/// <summary>
/// Depth-first sequence of this expr and its sub-expressions unless the predicate returns false.
/// In which case the sub-tree is skipped
/// </summary>
let depthFirstExprsUntilFalse (pred: SynExpr -> bool) (expr: SynExpr) =
    let rec main_loop (pred: SynExpr -> bool) (expr: SynExpr) =
        let loop = main_loop pred

        seq {
            yield expr

            if pred expr then
                match expr with
                | SynExpr.LetStar(bindings, body, _) ->
                    for SynBinding(_, value, _) in bindings do
                        yield! loop value

                    for b in body do
                        yield! loop b
                | SynExpr.ForIn(_, binding, body, _) ->
                    yield! loop binding

                    for b in body do
                        yield! loop b

                | SynExpr.RangeExpr(start, mid, last, _) ->
                    yield! loop start

                    match mid with
                    | Some m -> yield! loop m
                    | None -> ()

                    yield! loop last

                | SynExpr.If(cond, thn, alt, _) ->
                    yield! loop cond
                    yield! loop thn

                    match alt with
                    | Some a -> yield! loop a
                    | None -> ()

                | SynExpr.FunctionDef(_, _, _, args, _)
                | SynExpr.FunctionCall(_, args, _) ->
                    for arg in args do
                        yield! loop arg

                | SynExpr.Op(op) ->
                    match op with
                    | SynOp.Plus(args, _) ->
                        for arg in args do
                            yield! loop arg
                    | SynOp.Div(args, _) ->
                        for arg in args do
                            yield! loop arg
                    | SynOp.Minus(args, _) ->
                        for arg in args do
                            yield! loop arg
                    | SynOp.Mult(args, _) ->
                        for arg in args do
                            yield! loop arg

                | SynExpr.SimpleLet(_, expr, _)
                | SynExpr.SimpleMut(_, expr, _)
                | SynExpr.Atom(expr, _)
                | SynExpr.Deref(_, expr, _) -> yield! loop expr

                | SynExpr.Set(name, value, _) ->
                    yield! loop name
                    yield! loop value

                | SynExpr.FsYield(exprs, _, _) -> yield! loop exprs
                | SynExpr.LambdaDef(SynLambda(_, exprs, _))
                | SynExpr.Begin(exprs, _, _)
                | SynExpr.New(_, exprs, _)
                | SynExpr.Tuple(exprs, _)
                | SynExpr.FsSeq(exprs, _)
                | SynExpr.FsSet(exprs, _)
                | SynExpr.FsArray(exprs, _)
                | SynExpr.FsMap(exprs, _)
                | SynExpr.FsVec(exprs, _)
                | SynExpr.List(exprs, _)
                | SynExpr.Vector(exprs, _)
                | SynExpr.HashMap(exprs, _)
                | SynExpr.HashSet(exprs, _) ->
                    for e in exprs do
                        yield! loop e

                | SynExpr.Pair(lhs, rhs, _)
                | SynExpr.Concat(lhs, rhs, _)
                | SynExpr.Cons(lhs, rhs, _) ->
                    yield! loop lhs
                    yield! loop rhs
                | SynExpr.DotIndex(target, index, _) ->
                    yield! loop target
                    yield! loop index
                | SynExpr.DotProperty(target, _, _) -> yield! loop target
                | SynExpr.DotMethod(target, _, args, _, _) ->
                    yield! loop target

                    for e in args do
                        yield! loop e

                | SynExpr.While(cond, body, _) ->
                    yield! loop cond

                    for e in body do
                        yield! loop e
                | SynExpr.ThreadFirst(body, _) ->
                    for e in body do
                        yield! loop e
                | SynExpr.ThreadLast(body, _) ->
                    for it in body do
                        match it with
                        | SynThreadable.Expr(it, _) -> yield! loop it
                        | _ -> ()
                | SynExpr.MacroCall _ -> ()
                | SynExpr.MacroDef _ -> ()
                | SynExpr.SyntaxMacroCall _ -> ()
                | SynExpr.SyntaxMacro _ -> ()
                | SynExpr.Quote _ -> ()
                | SynExpr.Quasiquote _ -> ()
                | SynExpr.Const _ -> ()
                | SynExpr.Literal _ -> ()
                | SynExpr.Keyword _ -> ()
                | SynExpr.Symbol _ -> ()
                | SynExpr.TypeAlias _ -> ()
                | SynExpr.LambdaShort(call, _) -> yield! loop call
                | SynExpr.Match(expr, pats, _) ->
                    yield! loop expr

                    for SynMatch.SynMatch(_, cond, body, _) in pats do
                        match cond with
                        | Some it -> yield! loop it
                        | None -> ()

                        for e in body do
                            yield! loop e

                | SynExpr.RecordInit(inits, _) ->
                    for SynInit(expr = e) in inits do
                        yield! loop e

                | SynExpr.Union(_, _, members, attributes, _)
                | SynExpr.Record(_, _, members, attributes, _)
                | SynExpr.Type(_, _, members, attributes, _) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                    for mem in members do
                        match mem with
                        | SynTypeMember.Let(_, e, _)
                        | SynTypeMember.Mut(_, e, _)
                        | SynTypeMember.Member(_, e, _)
                        | SynTypeMember.OverrideMember(_, e, _) -> yield! loop e
                        | SynTypeMember.MemberFn(_, _, body, _)
                        | SynTypeMember.OverrideFn(_, _, body, _) ->
                            for e in body do
                                yield! loop e
        }

    main_loop pred expr

let alwaysTrue _ = true

/// <summary>
/// Depth-first sequence of this expr and its sub-expressions.
/// </summary>
let depthFirstExprs = depthFirstExprsUntilFalse alwaysTrue

let depthFirstExprsInFilePred (pred: SynExpr -> bool) (ParsedFile(fragments)) =
    let rec main_loop (pred: SynExpr -> bool) (decl: SynModuleDecl) =
        seq {
            match decl with
            | SynModuleDecl.Expr(ex, _) -> yield! depthFirstExprsUntilFalse pred ex
            | SynModuleDecl.HashDirective _
            | SynModuleDecl.Open _
            | SynModuleDecl.Require _
            | SynModuleDecl.ModuleAbbrev _ -> ()
            | SynModuleDecl.NestedModule(_, decls, _) ->
                for decl in decls do
                    yield! main_loop pred decl
        }


    seq {
        for (ParsedFileFragment.AnonModule(decls, _)) in fragments do
            for decl in decls do
                yield! main_loop pred decl
    }

let depthFirstExprsInFile = depthFirstExprsInFilePred alwaysTrue
