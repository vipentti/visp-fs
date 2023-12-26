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

        let rec loop_members (pred: SynExpr -> bool) (mem: SynTypeMember) =
            seq {
                match mem with
                | SynTypeMember.GetSet(_, get, set, _, attributes, _) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                    match get with
                    | None -> ()
                    | Some(SynMemberGet(_, exprs, _)) ->
                        for e in exprs do
                            yield! loop e

                    match set with
                    | None -> ()
                    | Some(SynMemberSet(_, _, exprs, _)) ->
                        for e in exprs do
                            yield! loop e

                | SynTypeMember.Val(attributes = attributes) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                | SynTypeMember.Let(_, e, _, attributes, _) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                    yield! loop e

                | SynTypeMember.Constructor(_, body, _) ->
                    for e in body do
                        yield! loop e

                | SynTypeMember.Member(_, body, _, attributes, _)
                | SynTypeMember.MemberFn(_, _, body, _, attributes, _) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                    for e in body do
                        yield! loop e

                | SynTypeMember.Interface(_, mems, _) ->
                    for m in mems do
                        yield! loop_members pred m
            }

        seq {
            yield expr

            if pred expr then
                match expr with
                | SynExpr.ObjectExpression(ctor, members, _) ->
                    match ctor with
                    | TypeOrCtor.Type _ -> ()
                    | TypeOrCtor.Ctor(_, args, _) ->
                        for b in args do
                            yield! loop b

                    for mem in members do
                        yield! loop_members pred mem

                | SynExpr.LetStar(bindings, body, _) ->
                    for SynBinding(_, value, _) in bindings do
                        yield! loop value

                    for b in body do
                        yield! loop b
                | SynExpr.ForTo(_, start, finish, body, _, _) ->
                    yield! loop start
                    yield! loop finish

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

                | SynExpr.FunctionDef(_, _, _, args, _, _) ->
                    for arg in args do
                        yield! loop arg

                | SynExpr.FunctionCall(ex, args, _) ->
                    yield! loop ex

                    for arg in args do
                        yield! loop arg

                | SynExpr.Op(op) ->
                    for arg in op.Exprs do
                        yield! loop arg

                | SynExpr.Atom(expr, _)
                | SynExpr.Deref(_, expr, _) -> yield! loop expr

                | SynExpr.Set(name, value, _) ->
                    yield! loop name
                    yield! loop value

                | SynExpr.Collection it ->
                    for e in it.Items do
                        yield! loop e

                | SynExpr.FsDo(exprs, _, _) -> yield! loop exprs
                | SynExpr.FsReturn(exprs, _, _) -> yield! loop exprs
                | SynExpr.FsYield(exprs, _, _) -> yield! loop exprs
                | SynExpr.LambdaDef(SynLambda(_, exprs, _))
                | SynExpr.Begin(exprs, _, _)
                | SynExpr.New(_, exprs, _)
                | SynExpr.Tuple(exprs, _)
                | SynExpr.Computation(_, exprs, _)
                | SynExpr.FsSeq(exprs, _) ->
                    for e in exprs do
                        yield! loop e

                | SynExpr.DotIndex(target, index, _) ->
                    yield! loop target

                    for e in index do
                        yield! loop e
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
                        | SynThreadable.Index(it, _) ->
                            for e in it do
                                yield! loop e
                        | _ -> ()
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

                | SynExpr.LetOrUse(_, expr, _, attributes, _) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                    yield! loop expr

                | SynExpr.Union(_, _, members, attributes, _)
                | SynExpr.Record(_, _, members, attributes, _)
                | SynExpr.Type(_, _, members, attributes, _) ->
                    for attrlist in attributes do
                        for attr in attrlist.Attributes do
                            yield! loop attr.ArgExpr

                    for mem in members do
                        yield! loop_members pred mem

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
