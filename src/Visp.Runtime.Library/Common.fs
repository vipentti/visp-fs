// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Runtime.Library

open System.IO

type IWriteTo =
    abstract member writeTo: TextWriter -> unit
