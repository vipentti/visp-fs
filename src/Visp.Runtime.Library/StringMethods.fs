// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

[<AutoOpen>]
module Visp.Runtime.Library.StringMethods

open Visp.Common
open SpanUtils.Extensions

let inline isMultilineString (str: string) =
    let mutable enu = str.EnumerateSplitLines()
    enu.MoveNext() && enu.MoveNext()

let normalizeIndent (str: string) =
    if not (isMultilineString str) then
        str
    else
        let mutable finalIndentLevel = 0

        let mutable lines = str.EnumerateSplitLines()

        while lines.MoveNext() do
            let cur = lines.Current
            let mutable enu = cur.GetEnumerator()
            let mutable isDone = false

            let mutable level = 0

            while not isDone && enu.MoveNext() do
                let ch = enu.Current

                if not (System.Char.IsWhiteSpace ch) then
                    isDone <- true
                else
                    level <- level + 1

                ()

            finalIndentLevel <- level
            ()

        if finalIndentLevel = 0 then
            str
        else
            let mutable sb = PooledStringBuilder.Get()
            ignore <| sb.EnsureCapacity(str.Length)
            let mutable lastNewline = false
            let mutable indent = 1
            let mutable enu = str.GetEnumerator()

            while (enu.MoveNext()) do
                let mutable ch = enu.Current

                if ch = '\n' && finalIndentLevel > 0 then
                    lastNewline <- true
                    indent <- 1
                    sb <- sb.Append ch
                else if lastNewline && ch = ' ' then
                    while (indent < finalIndentLevel && ch = ' ' && enu.MoveNext()) do
                        indent <- indent + 1
                        ch <- enu.Current
                        ()

                    if ch <> ' ' then
                        sb <- sb.Append ch

                    while ch = ' ' && enu.MoveNext() do
                        ch <- enu.Current
                        sb <- sb.Append ch
                        ()

                    lastNewline <- false
                else
                    lastNewline <- false
                    sb <- sb.Append ch

                ()

            sb.ToStringAndReturn()

let dedent = normalizeIndent
