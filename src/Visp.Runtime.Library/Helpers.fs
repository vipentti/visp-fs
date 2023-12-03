// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

module Visp.Runtime.Library.Helpers

let isTruthy (v: Value) =
    match v with
    | Value.Nil -> false
    | Value.Bool v -> v
    | _ -> true
