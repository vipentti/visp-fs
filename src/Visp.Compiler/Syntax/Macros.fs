// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler.Syntax

open System.Collections.Concurrent

type CompilerBuiltinMacro = SynMacroCall -> SynMacroBody

type MacroTable() =
    let macros = ConcurrentDictionary<string, SynMacro>()
    let builtinMacros = ConcurrentDictionary<string, CompilerBuiltinMacro>()
    let macroNames = ConcurrentDictionary<string, unit>()

    member _.IsMacro n =
        macroNames.ContainsKey n || macros.ContainsKey n || builtinMacros.ContainsKey n

    member _.TryGetMacro n =
        match macros.TryGetValue(n) with
        | false, _ -> None
        | true, m -> Some(m)

    member _.TryGetBuiltinMacro n =
        match builtinMacros.TryGetValue(n) with
        | true, m -> Some(m)
        | _ -> None

    member _.AddBuiltinMacro n m =
        lock builtinMacros (fun () -> builtinMacros[n] <- m)

    member _.AddMacro n m = lock macros (fun () -> macros[n] <- m)

    member _.AddMacroName n =
        lock macroNames (fun () -> macroNames[n] <- ())

module Macros =
    open Visp.Compiler.SyntaxPrinter
    open PrettyPrinter

    // ++GLOBAL MUTABLE STATE
    // WARNING: Global Mutable State, holding information about all recorded macros
    /// <summary>
    /// Holding information about known macros
    /// </summary>
    let macroTable = MacroTable()

    let inline private builtinMacroStringify (SynMacroCall(_, args, r)) =
        let docs = args |> List.tail |> List.map macroBodyToDoc |> Print.hsep
        SynMacroBody.Const(SynConst.String(docToStringPooled docs, SynStringKind.Regular, r), r)

    let inline private builtinMacroFile (SynMacroCall(_, _, r)) =
        SynMacroBody.Const(SynConst.String(FormatPath.normalizePath r.FileName, SynStringKind.Regular, r), r)

    let inline private builtinMacroLine (SynMacroCall(_, _, r)) =
        SynMacroBody.Const(SynConst.Int32(r.StartLine), r)

    do macroTable.AddBuiltinMacro "stringify!" builtinMacroStringify
    do macroTable.AddBuiltinMacro "file!" builtinMacroFile
    do macroTable.AddBuiltinMacro "line!" builtinMacroLine
