// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Runtime.Library

open System.IO

type IWriteTo =
    abstract member writeTo: TextWriter -> unit

module List =
    /// The intersperse function takes an element and a list and
    /// 'intersperses' that element between the elements of the list.
    let intersperse sep ls =
        List.foldBack
            (fun x ->
                function
                | [] -> [ x ]
                | xs -> x :: sep :: xs)
            ls
            []
