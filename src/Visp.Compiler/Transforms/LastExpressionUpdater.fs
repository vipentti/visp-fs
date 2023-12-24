// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler.Transforms

open Visp.Compiler.Syntax
open Visp.Compiler.Text

module LastExpressionUpdater =
    let rec update (ParsedFile fragments) =
        (updateLastFragment fragments) |> ParsedFile

    and private updateLastFragment (lst: ParsedFileFragment list) =
        match lst with
        | last :: [] -> [ updateFragment last ]
        | lhs :: rest -> lhs :: (updateLastFragment rest)
        | _ -> lst

    and private updateFragment (fragment: ParsedFileFragment) =
        match fragment with
        | ParsedFileFragment.AnonModule(decls, range) ->
            ParsedFileFragment.AnonModule((updateLastSynModuleDecl decls), range)

    and private updateLastSynModuleDecl (decls: SynModuleDecl list) =
        match decls with
        | last :: [] -> updateSynModuleDecl last
        | lhs :: rest -> lhs :: (updateLastSynModuleDecl rest)
        | _ -> decls

    and private updateSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.Expr(expr, range) ->
            [ SynModuleDecl.Expr(
                  SynExpr.LetOrUse(
                      Syntax.mkInferredNamePat "visp_result_todo" range,
                      expr,
                      LetFlags.None,
                      [],
                      range
                  ),
                  range
              )
              SynModuleDecl.Expr(
                  Syntax.mkFunctionCall
                      (Syntax.mkSynSymbolExpr "printfn" range)
                      [ Syntax.mkSynString "%A" range
                        Syntax.mkSynExprSymbol "visp_result_todo" range ]
                      range,
                  range
              ) ]
        | _ -> [ decl ]
