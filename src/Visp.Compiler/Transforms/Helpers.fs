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

        | SynExpr.ForTo(name, start, finish, body, down, range) ->
            SynExpr.ForTo(
                name,
                bound_transform start,
                bound_transform finish,
                List.map bound_transform body,
                down,
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
        | SynExpr.FunctionDef(name, is, args, body, retty, range) ->
            SynExpr.FunctionDef(name, is, args, List.map bound_transform body, retty, range)
        | SynExpr.FunctionCall(name, args, range) ->
            SynExpr.FunctionCall(bound_transform name, List.map bound_transform args, range)
        | SynExpr.LambdaDef(SynLambda(args, body, lambdaRange)) ->
            SynExpr.LambdaDef(SynLambda(args, List.map bound_transform body, lambdaRange))
        | SynExpr.Op op ->

            match op with
            | SynOp.Infix(o, args, range) -> SynOp.Infix(o, List.map bound_transform args, range)
            | SynOp.Unary(o, args, range) -> SynOp.Unary(o, List.map bound_transform args, range)
            |> SynExpr.Op

        | SynExpr.Atom(expr, range) -> SynExpr.Atom(bound_transform expr, range)
        | SynExpr.Deref(short, expr, range) -> SynExpr.Deref(short, bound_transform expr, range)
        | SynExpr.Begin(exprs, k, range) -> SynExpr.Begin(List.map bound_transform exprs, k, range)
        | SynExpr.New(typ, args, range) -> SynExpr.New(typ, List.map bound_transform args, range)
        | SynExpr.LetOrUse(name, value, flags, attributes, range) ->
            SynExpr.LetOrUse(
                name,
                bound_transform value,
                flags,
                fixAttributes bound_transform attributes,
                range
            )
        | SynExpr.Set(name, value, range) ->
            SynExpr.Set(bound_transform name, bound_transform value, range)
        | SynExpr.Tuple(exprs, range) -> SynExpr.Tuple(List.map bound_transform exprs, range)
        | SynExpr.FsReturn(exprs, b, range) -> SynExpr.FsReturn(bound_transform exprs, b, range)
        | SynExpr.FsYield(exprs, b, range) -> SynExpr.FsYield(bound_transform exprs, b, range)
        | SynExpr.FsDo(exprs, b, range) -> SynExpr.FsDo(bound_transform exprs, b, range)
        | SynExpr.Computation(it, exprs, range) ->
            SynExpr.Computation(it, List.map bound_transform exprs, range)
        | SynExpr.FsSeq(exprs, range) -> SynExpr.FsSeq(List.map bound_transform exprs, range)
        | SynExpr.Collection(SynCollection(kind, exprs, range)) ->
            SynExpr.Collection(SynCollection(kind, List.map bound_transform exprs, range))
        | SynExpr.DotIndex(target, index, range) ->
            SynExpr.DotIndex(bound_transform target, List.map bound_transform index, range)
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
                    | SynThreadable.Index(it, r) ->
                        SynThreadable.Index(List.map bound_transform it, r)
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
        | SynExpr.ObjectExpression(ctor, members, range) ->
            SynExpr.ObjectExpression(
                match ctor with
                | TypeOrCtor.Type _ -> ctor
                | TypeOrCtor.Ctor(it, args, r) ->
                    TypeOrCtor.Ctor(it, List.map bound_transform args, r)
                , fixMembers bound_transform members
                , range
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
    let tfGet (SynMemberGet(args, exprs, range)) =
        SynMemberGet(args, List.map bound_transform exprs, range)

    let tfSet (SynMemberSet(args, k, exprs, range)) =
        SynMemberSet(args, k, List.map bound_transform exprs, range)

    let fixAttrs = fixAttributes bound_transform

    let rec tfmember =
        function
        | SynTypeMember.Let(name, expr, flags, attributes, range) ->
            SynTypeMember.Let(name, bound_transform expr, flags, fixAttrs attributes, range)
        | SynTypeMember.Constructor(args, expr, range) ->
            SynTypeMember.Constructor(args, List.map bound_transform expr, range)
        | SynTypeMember.Val(name, typ, flags, attributes, range) ->
            SynTypeMember.Val(name, typ, flags, fixAttrs attributes, range)
        | SynTypeMember.Member(name, expr, flags, attributes, range) ->
            SynTypeMember.Member(
                name,
                List.map bound_transform expr,
                flags,
                fixAttrs attributes,
                range
            )
        | SynTypeMember.MemberFn(name, args, expr, flags, attributes, range) ->
            SynTypeMember.MemberFn(
                name,
                args,
                List.map bound_transform expr,
                flags,
                fixAttrs attributes,
                range
            )
        | SynTypeMember.GetSet(name, get, set, flags, attributes, range) ->
            SynTypeMember.GetSet(
                name,
                Option.map tfGet get,
                Option.map tfSet set,
                flags,
                fixAttrs attributes,
                range
            )
        | SynTypeMember.Interface(name, mems, r) ->
            SynTypeMember.Interface(name, List.map tfmember mems, r)


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
        | SynModuleDecl.Include _ -> ()
        | SynModuleDecl.Open _ -> ()
        | SynModuleDecl.Require(name, ver, _) -> res.Add(Require(Syntax.textOfSymbol name, ver))
        | SynModuleDecl.ModuleAbbrev _ -> ()
        | SynModuleDecl.ModuleList(decls, _) -> List.iter (transformModuleDecl res) decls
        | SynModuleDecl.IncludedModule(_, decls, _) -> List.iter (transformModuleDecl res) decls
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
        | SynModuleDecl.Include _ as it -> it
        | SynModuleDecl.ModuleAbbrev _ as it -> it
        | SynModuleDecl.ModuleList(decls, range) ->
            SynModuleDecl.ModuleList(List.map transformModuleDecl decls, range)
        | SynModuleDecl.IncludedModule(path, decls, range) ->
            SynModuleDecl.IncludedModule(path, List.map transformModuleDecl decls, range)
        | SynModuleDecl.NestedModule(name, decls, range) ->
            SynModuleDecl.NestedModule(name, List.map transformModuleDecl decls, range)

    let transformFragment (frag: ParsedFileFragment) =
        let (ParsedFileFragment.AnonModule(decls, range)) = frag

        ParsedFileFragment.AnonModule(List.map transformModuleDecl decls, range)

    let (ParsedFile(fragments)) = file
    ParsedFile(List.map transformFragment fragments)

let transformSynModuleDecls (func: SynModuleDecl -> SynModuleDecl) (file: ParsedFile) =

    let rec transformModuleDecl (func: SynModuleDecl -> SynModuleDecl) (decl: SynModuleDecl) =
        let bound_transform = transformModuleDecl func

        let result =
            match decl with
            | SynModuleDecl.Expr(_) as it -> it
            | SynModuleDecl.HashDirective _ as it -> it
            | SynModuleDecl.Open _ as it -> it
            | SynModuleDecl.Require _ as it -> it
            | SynModuleDecl.Include _ as it -> it
            | SynModuleDecl.ModuleAbbrev _ as it -> it
            | SynModuleDecl.ModuleList(decls, range) ->
                SynModuleDecl.ModuleList(List.map bound_transform decls, range)
            | SynModuleDecl.IncludedModule(path, decls, range) ->
                SynModuleDecl.IncludedModule(path, List.map bound_transform decls, range)
            | SynModuleDecl.NestedModule(name, decls, range) ->
                SynModuleDecl.NestedModule(name, List.map bound_transform decls, range)

        func result

    let transformFragment func (frag: ParsedFileFragment) =
        let (ParsedFileFragment.AnonModule(decls, range)) = frag

        ParsedFileFragment.AnonModule(List.map (transformModuleDecl func) decls, range)

    let (ParsedFile(fragments)) = file
    ParsedFile(List.map (transformFragment func) fragments)
