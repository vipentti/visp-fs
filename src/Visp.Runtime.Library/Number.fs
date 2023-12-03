// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Runtime.Library

open System.Globalization
open System
open System.IO

module ImplHelper =
    let toInvariantString (s: obj) =
        System.Convert.ToString(s, CultureInfo.InvariantCulture)

module TryMath =
    let inline tryAdd (lhs: int64) (rhs: int64) : option<int64> =
        let res = Operators.(+) lhs rhs

        let overflowBit =
            (int64 (uint64 (lhs ^^^ res) &&& ~~~(uint64 (lhs ^^^ rhs))) >>> 63)

        if overflowBit = 0 then Some(res) else None

    let inline tryMul (lhs: int64) (rhs: int64) : option<int64> =
        let res = Operators.(*) lhs rhs
        let didOverflow = lhs <> 0 && Int64.MaxValue / lhs < rhs
        if not didOverflow then Some(res) else None

    let inline trySub (lhs: int64) (rhs: int64) : option<int64> =
        let res = Operators.(-) lhs rhs
        let didOverflow = (res < lhs) <> (rhs > 0)
        if not didOverflow then Some(res) else None

[<Struct; RequireQualifiedAccess>]
type Number =
    | Int64 of int: int64
    | Decimal of dec: decimal
    | BigInt of big: bigint

    static member big(it: bigint) = BigInt it
    static member int(it: int64) = Int64 it
    static member decimal(it: decimal) = Decimal it

    static member from(it: int32) = Int64 it
    static member from(it: int64) = Int64 it
    static member from(it: decimal) = Decimal it
    static member from(it: Number) = it

    static member ParseOption(str: string) =
        let span = str.AsSpan()

        if span.Contains('.') then
            match System.Decimal.TryParse(span, CultureInfo.InvariantCulture) with
            | true, n -> Some(Number.Decimal n)
            | _ -> None
        else
            match System.Int64.TryParse(span, CultureInfo.InvariantCulture) with
            | true, n -> Some(Number.Int64 n)
            | _ -> None

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            match this with
            | Int64 it -> writer.Write(it)
            | BigInt it -> writer.Write(it)
            | Decimal it -> writer.Write(ImplHelper.toInvariantString it)

            ()

    static member private i64plus a b =
        match TryMath.tryAdd a b with
        | Some(r) -> Int64(r)
        | _ -> BigInt((bigint a) + (bigint b))

    static member (+)(lhs: Number, rhs: Number) =
        match (lhs, rhs) with
        | (Int64 lhs, Int64 rhs) -> (Number.i64plus lhs rhs)
        | (Decimal lhs, Decimal rhs) -> (Decimal(lhs + rhs))
        | (BigInt lhs, BigInt rhs) -> (BigInt(lhs + rhs))
        | _ ->
            match Number.CoerceNumbers(lhs, rhs) with
            | (Int64 lhs, Int64 rhs) -> (Number.i64plus lhs rhs)
            | (Decimal lhs, Decimal rhs) -> (Decimal(lhs + rhs))
            | (BigInt lhs, BigInt rhs) -> (BigInt(lhs + rhs))
            | _ -> failwithf "Unsupported op %O %O" lhs rhs

    static member private i64sub a b =
        match TryMath.trySub a b with
        | Some(r) -> Int64(r)
        | _ -> BigInt((bigint a) - (bigint b))

    static member (-)(lhs: Number, rhs: Number) =
        match (lhs, rhs) with
        | (Int64 lhs, Int64 rhs) -> (Number.i64sub lhs rhs)
        | (Decimal lhs, Decimal rhs) -> (Decimal(lhs - rhs))
        | (BigInt lhs, BigInt rhs) -> (BigInt(lhs - rhs))
        | _ ->
            match Number.CoerceNumbers(lhs, rhs) with
            | (Int64 lhs, Int64 rhs) -> (Number.i64sub lhs rhs)
            | (Decimal lhs, Decimal rhs) -> (Decimal(lhs - rhs))
            | (BigInt lhs, BigInt rhs) -> (BigInt(lhs - rhs))
            | _ -> failwithf "Unsupported op %O %O" lhs rhs

    static member private i64mul a b =
        match TryMath.tryMul a b with
        | Some(r) -> Int64(r)
        | _ -> BigInt((bigint a) * (bigint b))

    static member (*)(lhs: Number, rhs: Number) =
        match (lhs, rhs) with
        | (Int64 lhs, Int64 rhs) -> (Number.i64mul lhs rhs)
        | (Decimal lhs, Decimal rhs) -> (Decimal(lhs * rhs))
        | (BigInt lhs, BigInt rhs) -> (BigInt(lhs * rhs))
        | _ ->
            match Number.CoerceNumbers(lhs, rhs) with
            | (Int64 lhs, Int64 rhs) -> (Number.i64mul lhs rhs)
            | (Decimal lhs, Decimal rhs) -> (Decimal(lhs * rhs))
            | (BigInt lhs, BigInt rhs) -> (BigInt(lhs * rhs))
            | _ -> failwithf "Unsupported op %O %O" lhs rhs


    static member (/)(lhs: Number, rhs: Number) =
        match (lhs, rhs) with
        | (Int64 lhs, Int64 rhs) -> (Int64(lhs / rhs))
        | (Decimal lhs, Decimal rhs) -> (Decimal(lhs / rhs))
        | (BigInt lhs, BigInt rhs) -> (BigInt(lhs / rhs))
        | _ ->
            match Number.CoerceNumbers(lhs, rhs) with
            | (Int64 lhs, Int64 rhs) -> (Int64(lhs / rhs))
            | (Decimal lhs, Decimal rhs) -> (Decimal(lhs / rhs))
            | (BigInt lhs, BigInt rhs) -> (BigInt(lhs / rhs))
            | _ -> failwithf "Unsupported op %O %O" lhs rhs

    static member CoerceNumbers(lhs: Number, rhs: Number) =
        match (lhs, rhs) with
        | (Int64 _, Int64 _) -> (lhs, rhs)
        | (Decimal _, Decimal _) -> (lhs, rhs)
        | (BigInt _, BigInt _) -> (lhs, rhs)
        | (Int64 a, Decimal b) -> (Decimal(decimal a), Decimal b)
        | (Decimal a, Int64 b) -> (Decimal a, Decimal(decimal b))
        | (BigInt a, Int64 b) -> (BigInt a, BigInt(bigint b))
        | (Int64 a, BigInt b) -> (BigInt(bigint a), BigInt b)
        | _ -> (lhs, rhs)
