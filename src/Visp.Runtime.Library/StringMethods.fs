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
            let len = cur.Length

            let mutable isDone = false
            let mutable level = 0
            let mutable index = 0

            while not isDone && index < len do
                let ch = cur[index]
                index <- index + 1

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
            let len = str.Length
            let mutable sb = PooledStringBuilder.Get()
            ignore <| sb.EnsureCapacity(len)
            let mutable lastNewline = false
            let mutable indent = 1
            let mutable index = 0

            while index < len do
                let mutable ch = str[index]
                index <- index + 1

                if ch = '\n' && finalIndentLevel > 0 then
                    lastNewline <- true
                    indent <- 1
                    sb <- sb.Append ch
                else if lastNewline && ch = ' ' then
                    while (indent < finalIndentLevel && ch = ' ' && index < len) do
                        ch <- str[index]
                        indent <- indent + 1
                        index <- index + 1
                        ()

                    if ch <> ' ' then
                        sb <- sb.Append ch

                    while ch = ' ' && index < len do
                        ch <- str[index]
                        sb <- sb.Append ch
                        index <- index + 1
                        ()

                    lastNewline <- false
                else
                    lastNewline <- false
                    sb <- sb.Append ch

                ()

            sb.ToStringAndReturn()

let dedent = normalizeIndent
