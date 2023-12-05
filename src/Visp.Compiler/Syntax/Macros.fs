// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler.Syntax

open System.Collections.Concurrent

type MacroTable() =
    let macros = ConcurrentDictionary<string, SynMacro>()
    let macroNames = ConcurrentDictionary<string, unit>()

    member _.IsMacro n =
        macroNames.ContainsKey n || macros.ContainsKey n

    member _.TryGetMacro n =
        match macros.TryGetValue(n) with
        | false, _ -> None
        | true, m -> Some(m)

    member _.AddMacro n m = lock macros (fun () -> macros[n] <- m)

    member _.AddMacroName n =
        lock macroNames (fun () -> macroNames[n] <- ())

module Macros =
    // ++GLOBAL MUTABLE STATE
    // WARNING: Global Mutable State, holding information about all recorded macros
    /// <summary>
    /// Holding information about known macros
    /// </summary>
    let macroTable = MacroTable()
