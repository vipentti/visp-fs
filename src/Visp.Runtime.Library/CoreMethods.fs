// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Runtime.Library

open System

module private CompareHelpers =
    let inline allSatisfy<'a> ([<InlineIfLambda>] pred: 'a -> 'a -> bool) (values: 'a seq) =
        let mutable enu = values.GetEnumerator()

        if not (enu.MoveNext()) then
            failwith "empty enumerator"

        let mutable current = enu.Current
        let mutable isDone = false
        let mutable result = true

        while not isDone && enu.MoveNext() do
            let next = enu.Current

            if not (pred current next) then
                result <- false
                isDone <- true
            else
                current <- next

        result

[<AutoOpen>]
module Bitwise =
    let inline bor (lhs: 'a) (rhs: 'a) = lhs ||| rhs
    let inline band (lhs: 'a) (rhs: 'a) = lhs &&& rhs
    let inline bcompl (lhs: 'a) = ~~~lhs


type CoreMethods =
    static member ``vector-push!``(_: RuntimeState, [<ParamArray>] args: Value[]) =
        match Array.tryHead args with
        | Some(Value.Vector vec) ->
            let slice = args.AsSpan(1)
            vec.AddRange(slice)
            Value.Vector vec
        | _ -> failwith "invalid vector-push!"

    static member str(_: RuntimeState, [<ParamArray>] args: Value[]) =
        match Array.tryHead args with
        | Some(v) -> Value.string (sprintf "%O" v)
        | _ -> failwith "invalid vector-push!"

    static member ``swap!``(value: Value, method: Value -> Value) : Value =
        let atom = unwrapAtom value
        atom.Value <- method atom.Value
        Value.from atom

    static member inline add([<ParamArray>] args: 'a[]) : 'a = args |> Array.reduce (+)

    static member inline sub([<ParamArray>] args: 'a[]) : 'a = args |> Array.reduce (-)

    static member inline mul([<ParamArray>] args: 'a[]) : 'a = args |> Array.reduce (*)

    static member inline div([<ParamArray>] args: 'a[]) : 'a = args |> Array.reduce (/)

    static member inline rem_impl (lhs: 'a) (rhs: 'a) : 'a = lhs % rhs

    static member inline ``null?``<'a when 'a: null and 'a: equality>(v: 'a) = v = null

    static member ``eq?``<'a when 'a: equality>(lhs: 'a, rhs: 'a) = lhs = rhs

    static member ``neq?``<'a when 'a: equality>(lhs: 'a, rhs: 'a) = lhs <> rhs

    static member ``all-eq?``<'a when 'a: equality>([<ParamArray>] args: 'a[]) =
        CompareHelpers.allSatisfy (=) args

    static member lt<'a when 'a: comparison>([<ParamArray>] args: 'a[]) =
        CompareHelpers.allSatisfy (<) args

    static member lte<'a when 'a: comparison>([<ParamArray>] args: 'a[]) =
        CompareHelpers.allSatisfy (<=) args

    static member gt<'a when 'a: comparison>([<ParamArray>] args: 'a[]) =
        CompareHelpers.allSatisfy (>) args

    static member gte<'a when 'a: comparison>([<ParamArray>] args: 'a[]) =
        CompareHelpers.allSatisfy (>=) args

    static member gt_num([<ParamArray>] args: Number[]) = CompareHelpers.allSatisfy (<) args

    static member ``i64->``([<ParamArray>] args: int64[]) = CompareHelpers.allSatisfy (>) args

    static member ``i64-=``([<ParamArray>] args: int64[]) = CompareHelpers.allSatisfy (=) args

    static member inline ``i64-+``(v: int64, b: int64) = v + b

    static member inline isTruthy(v: Value) =
        match v with
        | Value.Nil -> false
        | Value.Bool v -> v
        | _ -> true

    static member inline isTruthy(v: bool) = v

    static member inline not(v: Value) = not (CoreMethods.isTruthy v)

    static member inline not(v: bool) = not v

    static member chunks((s: seq<'v>), (size: int)) : seq<list<'v>> =
        seq {
            let en = s.GetEnumerator()
            let more = ref true

            while more.Value do
                let group =
                    [ let i = ref 0

                      while i.Value < size && en.MoveNext() do
                          yield en.Current
                          i.Value <- i.Value + 1 ]

                if List.isEmpty group then
                    more.Value <- false
                else
                    yield group
        }



module CompileHelpers =
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open System.Text
    open System.Collections.Generic

    let runtimeStateType = typeof<RuntimeState>
    let valueListType = typeof<Value list>
    let valueArrayType = typeof<Value array>
    let argType = typeof<RuntimeState * Value array>

    let hasSingleValueArrayTypeArg (m: Reflection.MethodInfo) =
        let parameters = m.GetParameters()

        parameters.Length = 1 && parameters.[0].ParameterType = valueArrayType

    // let hasSingleParamArrayTypeArg (m: Reflection.MethodInfo) =
    //     let parameters = m.GetParameters()

    //     if parameters.Length = 1 then
    //         if parameters.[0]

    //         false
    //     else
    //         false
    let hasParamArrayAttribute (methodInfo: Reflection.MethodInfo) =
        methodInfo.GetParameters()
        |> Array.exists (fun p ->
            p.GetCustomAttributes(typeof<ParamArrayAttribute>, false) |> Array.length > 0)


    let isVariableArgMethod (m: Reflection.MethodInfo) =
        let parameters = m.GetParameters()

        parameters.Length = 2
        && parameters.[0].ParameterType = runtimeStateType
        && parameters.[1].ParameterType = valueArrayType

    let getMethods () =
        let methods = typeof<CoreMethods>.GetMethods() |> List.ofArray

        let map = methods |> List.map (fun m -> (m.Name, m)) |> Map.ofList

        let extras =
            [ ("=", nameof CoreMethods.``eq?``)
              ("!=", nameof CoreMethods.``neq?``)
              ("<", nameof CoreMethods.lt)
              (">", nameof CoreMethods.gt)
              ("<=", nameof CoreMethods.lte)
              (">=", nameof CoreMethods.gte)
              ("+", nameof CoreMethods.add)
              ("-", nameof CoreMethods.sub)
              ("*", nameof CoreMethods.mul)
              ("/", nameof CoreMethods.div) ]

        extras |> List.fold (fun map (lhs, rhs) -> Map.add lhs (map[rhs]) map) map

    let methodSourceName (mi: MemberInfo) =
        mi.GetCustomAttributes(true)
        |> Array.tryPick (function
            | :? CompilationSourceNameAttribute as csna -> Some(csna)
            | _ -> None)
        |> (function
        | Some(csna) -> csna.SourceName
        | None -> mi.Name)


    // based on https://stackoverflow.com/a/51831760

    let rec fsharpFunSourceOf (t: Type) =
        let rec loop nested t =
            if FSharpType.IsTuple t then
                FSharpType.GetTupleElements t |> Array.map (loop true) |> String.concat " * "
            elif FSharpType.IsFunction t then
                let fs = if nested then sprintf "(%s -> %s)" else sprintf "%s -> %s"
                let domain, range = FSharpType.GetFunctionElements t
                fs (loop true domain) (loop false range)
            else
                t.FullName

        loop false t

    let fsharpFunSource o = fsharpFunSourceOf (o.GetType())

    let rec signatureOf (mi: MethodInfo) =
        let sb = new StringBuilder()
        let ps = mi.GetParameters()

        let mutable f = true

        if ps.Length = 0 then
            sb.Append("()") |> ignore
        else
            let mutable i = 0
            let dict = new Dictionary<Type, string>()

            for p in ps do
                if f then f <- false else sb.Append(" -> ") |> ignore

                if FSharpType.IsFunction p.ParameterType then
                    sb.Append("(a -> b)") |> ignore
                else
                    match dict.TryGetValue(p.ParameterType) with
                    | false, _ ->
                        let typ = "a" + (i.ToString())
                        sb.Append(typ) |> ignore
                        dict[p.ParameterType] <- typ
                    | true, typ -> sb.Append(typ) |> ignore

                i <- i + 1

        sb.Append(" -> ") |> ignore
        sb.Append("ret") |> ignore
        sb.ToString()

    let fsharpCoreAssembly = typeof<Collections.List<_>>.Assembly

    let getFsharpCollectionModuleMethods (name: string) =
        // Get the type information for the module
        let modu = fsharpCoreAssembly.GetType($"Microsoft.FSharp.Collections.{name}Module")

        // Get the methods of the module
        let methods = modu.GetMethods()

        methods |> Array.map (fun m -> (name, methodSourceName m, signatureOf m))

    let getFsharpListMethods () =
        Array.concat
            [| getFsharpCollectionModuleMethods "Array"
               getFsharpCollectionModuleMethods "Array2D"
               getFsharpCollectionModuleMethods "List"
               getFsharpCollectionModuleMethods "Map"
               getFsharpCollectionModuleMethods "Seq"
               getFsharpCollectionModuleMethods "Set" |]
