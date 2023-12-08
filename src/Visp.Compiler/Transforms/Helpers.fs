// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Compiler.Transforms.Helpers

open Visp.Compiler.Syntax

let rec transform (func: SynExpr -> SynExpr) expr =
    let bound_transform = transform func

    let result =
        match expr with
        | SynExpr.LetStar(bindings, body, range) ->
            SynExpr.LetStar(
                List.map
                    (fun (SynBinding(name, value, range)) ->
                        SynBinding(name, bound_transform value, range))
                    bindings,
                List.map bound_transform body,
                range
            )

        | SynExpr.ForIn(name, binding, body, range) ->
            SynExpr.ForIn(name, bound_transform binding, List.map bound_transform body, range)

        | SynExpr.RangeExpr(start, mid, last, range) ->
            SynExpr.RangeExpr(
                bound_transform start,
                Option.map bound_transform mid,
                bound_transform last,
                range
            )

        | SynExpr.If(cond, thn, alt, range) ->
            SynExpr.If(
                bound_transform cond,
                bound_transform thn,
                Option.map bound_transform alt,
                range
            )
        | SynExpr.MacroDef(name, args, body, range) ->
            SynExpr.MacroDef(name, args, List.map bound_transform body, range)
        | SynExpr.MacroCall _ -> expr
        | SynExpr.FunctionDef(name, is, args, body, range) ->
            SynExpr.FunctionDef(name, is, args, List.map bound_transform body, range)
        | SynExpr.FunctionCall(name, args, range) ->
            SynExpr.FunctionCall(bound_transform name, List.map bound_transform args, range)
        | SynExpr.LambdaDef(SynLambda(args, body, lambdaRange)) ->
            SynExpr.LambdaDef(SynLambda(args, List.map bound_transform body, lambdaRange))
        | SynExpr.Op op ->

            match op with
            | SynOp.Plus(args, range) -> SynOp.Plus(List.map bound_transform args, range)
            | SynOp.Div(args, range) -> SynOp.Div(List.map bound_transform args, range)
            | SynOp.Minus(args, range) -> SynOp.Minus(List.map bound_transform args, range)
            | SynOp.Mult(args, range) -> SynOp.Mult(List.map bound_transform args, range)
            |> SynExpr.Op

        | SynExpr.Cons(lhs, rhs, range) ->
            SynExpr.Cons(bound_transform lhs, bound_transform rhs, range)
        | SynExpr.Concat(lhs, rhs, range) ->
            SynExpr.Concat(bound_transform lhs, bound_transform rhs, range)
        | SynExpr.Atom(expr, range) -> SynExpr.Atom(bound_transform expr, range)
        | SynExpr.Deref(short, expr, range) -> SynExpr.Deref(short, bound_transform expr, range)
        | SynExpr.Begin(exprs, k, range) -> SynExpr.Begin(List.map bound_transform exprs, k, range)
        | SynExpr.New(typ, args, range) -> SynExpr.New(typ, List.map bound_transform args, range)
        | SynExpr.SimpleLet(name, value, range) ->
            SynExpr.SimpleLet(name, bound_transform value, range)
        | SynExpr.SimpleMut(name, value, range) ->
            SynExpr.SimpleMut(name, bound_transform value, range)
        | SynExpr.Set(name, value, range) ->
            SynExpr.Set(bound_transform name, bound_transform value, range)
        | SynExpr.Pair(lhs, rhs, range) ->
            SynExpr.Pair(bound_transform lhs, bound_transform rhs, range)
        | SynExpr.Tuple(exprs, range) -> SynExpr.Tuple(List.map bound_transform exprs, range)
        | SynExpr.FsYield(exprs, b, range) -> SynExpr.FsYield(bound_transform exprs, b, range)
        | SynExpr.FsSeq(exprs, range) -> SynExpr.FsSeq(List.map bound_transform exprs, range)
        | SynExpr.FsSet(exprs, range) -> SynExpr.FsSet(List.map bound_transform exprs, range)
        | SynExpr.FsArray(exprs, range) -> SynExpr.FsArray(List.map bound_transform exprs, range)
        | SynExpr.FsMap(exprs, range) -> SynExpr.FsMap(List.map bound_transform exprs, range)
        | SynExpr.FsVec(exprs, range) -> SynExpr.FsVec(List.map bound_transform exprs, range)
        | SynExpr.List(exprs, range) -> SynExpr.List(List.map bound_transform exprs, range)
        | SynExpr.Vector(exprs, range) -> SynExpr.Vector(List.map bound_transform exprs, range)
        | SynExpr.HashMap(exprs, range) -> SynExpr.HashMap(List.map bound_transform exprs, range)
        | SynExpr.HashSet(exprs, range) -> SynExpr.HashSet(List.map bound_transform exprs, range)
        | SynExpr.DotIndex(target, index, range) ->
            SynExpr.DotIndex(bound_transform target, bound_transform index, range)
        | SynExpr.DotProperty(target, property, range) ->
            SynExpr.DotProperty(bound_transform target, property, range)
        | SynExpr.DotMethod(target, method, args, kind, range) ->
            SynExpr.DotMethod(
                bound_transform target,
                method,
                List.map bound_transform args,
                kind,
                range
            )
        | SynExpr.While(cond, body, range) ->
            SynExpr.While(bound_transform cond, List.map bound_transform body, range)
        | SynExpr.ThreadFirst(body, range) ->
            SynExpr.ThreadFirst(List.map bound_transform body, range)
        | SynExpr.ThreadLast(body, range) ->
            SynExpr.ThreadLast(
                List.map
                    (function
                    | SynThreadable.Expr(it, r) -> SynThreadable.Expr(bound_transform it, r)
                    | it -> it)
                    body,
                range
            )
        | SynExpr.SyntaxMacroCall _ -> expr
        | SynExpr.SyntaxMacro _ -> expr
        | SynExpr.Quote _ -> expr
        | SynExpr.Quasiquote _ -> expr
        | SynExpr.Const _ -> expr
        | SynExpr.Literal _ -> expr
        | SynExpr.Keyword _ -> expr
        | SynExpr.Symbol _ -> expr
        | SynExpr.TypeAlias _ -> expr
        | SynExpr.LambdaShort(call, range) -> SynExpr.LambdaShort(bound_transform call, range)
        | SynExpr.Match(expr, pats, range) ->
            SynExpr.Match(
                bound_transform expr,
                pats
                |> List.map (fun (SynMatch.SynMatch(pat, cond, body, range)) ->
                    SynMatch.SynMatch(
                        pat,
                        Option.map bound_transform cond,
                        List.map bound_transform body,
                        range
                    )),
                range
            )

        | SynExpr.RecordInit(inits, range) ->
            SynExpr.RecordInit(
                inits |> List.map (fun (SynInit(n, ex, r)) -> SynInit(n, bound_transform ex, r)),
                range
            )

        | SynExpr.Record(name, args, members, attributes, range) ->
            SynExpr.Record(
                name,
                args,
                fixMembers bound_transform members,
                fixAttributes bound_transform attributes,
                range
            )
        | SynExpr.Union(name, args, members, attributes, range) ->
            SynExpr.Union(
                name,
                args,
                fixMembers bound_transform members,
                fixAttributes bound_transform attributes,
                range
            )
        | SynExpr.Type(name, args, members, attributes, range) ->
            SynExpr.Type(
                name,
                args,
                fixMembers bound_transform members,
                fixAttributes bound_transform attributes,
                range
            )

    func result

and private fixAttributes bound_transform attributes =
    attributes
    |> List.map (fun it ->
        let items = it.Attributes

        let newAttributes =
            items
            |> List.map (fun attr ->
                let expr = bound_transform attr.ArgExpr
                { attr with ArgExpr = expr })

        { it with Attributes = newAttributes })


and private fixMembers bound_transform members =
    let tfmember =
        function
        | SynTypeMember.Let(name, expr, range) ->
            SynTypeMember.Let(name, bound_transform expr, range)
        | SynTypeMember.Mut(name, expr, range) ->
            SynTypeMember.Mut(name, bound_transform expr, range)
        | SynTypeMember.Member(name, expr, range) ->
            SynTypeMember.Member(name, bound_transform expr, range)
        | SynTypeMember.MemberFn(name, args, expr, range) ->
            SynTypeMember.MemberFn(name, args, List.map bound_transform expr, range)
        | SynTypeMember.OverrideMember(name, expr, range) ->
            SynTypeMember.OverrideMember(name, bound_transform expr, range)
        | SynTypeMember.OverrideFn(name, args, expr, range) ->
            SynTypeMember.OverrideFn(name, args, List.map bound_transform expr, range)

    members |> List.map tfmember


let runTransforms1 (expr: SynExpr) (tfs: (SynExpr -> SynExpr) seq) =
    let flip f a b = f b a
    tfs |> Seq.fold (flip transform) expr

let runTransforms (tfs: (SynExpr -> SynExpr) seq) (expr: SynExpr) =
    let flip f a b = f b a
    tfs |> Seq.fold (flip transform) expr

type Require = Require of name: string * version: string

let getAllRequires (file: ParsedFile) =
    let results = ResizeArray<_>()

    let rec transformModuleDecl (res: ResizeArray<_>) (decl: SynModuleDecl) =

        match decl with
        | SynModuleDecl.HashDirective _ -> ()
        | SynModuleDecl.Expr _ -> ()
        | SynModuleDecl.Open _ -> ()
        | SynModuleDecl.Require(name, ver, _) -> res.Add(Require(Syntax.textOfSymbol name, ver))
        | SynModuleDecl.ModuleAbbrev _ -> ()
        | SynModuleDecl.NestedModule(_, decls, _) -> List.iter (transformModuleDecl res) decls

        ()

    let transformFragment res (frag: ParsedFileFragment) =
        let (ParsedFileFragment.AnonModule(decls, _)) = frag
        List.iter (transformModuleDecl res) decls
        ()

    let (ParsedFile(fragments)) = file
    List.iter (transformFragment results) fragments

    results |> Set.ofSeq

let transformParsedFile (func: SynExpr -> SynExpr) (file: ParsedFile) =
    let bound_transform = transform func

    let rec transformModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.Expr(ex, r) -> SynModuleDecl.Expr(bound_transform ex, r)
        | SynModuleDecl.HashDirective _ as it -> it
        | SynModuleDecl.Open _ as it -> it
        | SynModuleDecl.Require _ as it -> it
        | SynModuleDecl.ModuleAbbrev _ as it -> it
        | SynModuleDecl.NestedModule(name, decls, range) ->
            SynModuleDecl.NestedModule(name, List.map transformModuleDecl decls, range)

    let transformFragment (frag: ParsedFileFragment) =
        let (ParsedFileFragment.AnonModule(decls, range)) = frag

        ParsedFileFragment.AnonModule(List.map transformModuleDecl decls, range)

    let (ParsedFile(fragments)) = file
    ParsedFile(List.map transformFragment fragments)
