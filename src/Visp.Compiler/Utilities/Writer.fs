// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Compiler

open System
open System.IO
open System.Globalization

module Writer =
    type CustomFileWriter(streamWriter: TextWriter, indentationFactor: int, commentPrefix: string) =
        let mutable indentation = 0

        member _.IndentLevel = indentation * indentationFactor

        member this.WriteSpaces(count: int32) =
            for _ in 0 .. count - 1 do
                streamWriter.Write(' ')

        member _.Inner = streamWriter

        member this.WriteIndent() =
            this.WriteSpaces(indentation * indentationFactor)

        member this.WriteLine(text: string) = streamWriter.WriteLine(text)

        member this.WriteLine() = streamWriter.WriteLine()

        member this.Write(text: string) = streamWriter.Write(text)

        member this.Write(text: char) = streamWriter.Write(text)

        member this.Write(it: int32) = streamWriter.Write(it)

        member this.Write(it: decimal) =
            streamWriter.Write(it.ToString(CultureInfo.InvariantCulture))

        member this.WriteSpace() = streamWriter.Write(' ')

        member this.WriteComment(text: string) =
            this.WriteIndent()
            streamWriter.Write(commentPrefix)
            streamWriter.Write(" ")
            streamWriter.Write(text)

        member this.WriteFormat format = fprintf streamWriter format
        member this.WriteFormatLine format = fprintfn streamWriter format

        //member this.WriteFormat (fmt: Printf.TextWriterFormat<'T>) (args: 'T) =
        //    fprintf streamWriter fmt args

        member _.IncreaseIndent() = indentation <- indentation + 1

        member _.DecreaseIndent() = indentation <- indentation - 1

        member this.Indent() =
            indentation <- indentation + 1

            { new IDisposable with
                member __.Dispose() = indentation <- indentation - 1 }

        member this.DoNotIndent() =
            { new IDisposable with
                member __.Dispose() = () }

    type DelegateDisposable(cleanup: Action) =
        interface IDisposable with
            member __.Dispose() = cleanup.Invoke()

        static member CreateBracket(setup: (unit -> unit) option, cleanup: (unit -> unit) option) =
            setup |> Option.iter (fun s -> s ())
            let cleanupAction = Option.defaultValue (fun () -> ()) cleanup
            new DelegateDisposable(cleanupAction)
