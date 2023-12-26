// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Runtime.Library

open Visp.Common
open System.Globalization
open System
open System.IO
open System.Collections.Generic

module Impl =
    let toInvariantString (s: obj) =
        System.Convert.ToString(s, CultureInfo.InvariantCulture)

[<CustomEquality; CustomComparison; RequireQualifiedAccess>]
type Value =
    | Nil
    | Unit
    | String of string
    | Symbol of string
    | Keyword of string
    | Number of Number
    | Char of char
    | Bool of bool
    | Any of AnyValue
    | Atom of Atom
    | List of ValueList
    | Vector of Vector
    | HashMap of HashMap
    | HashSet of HashSet
    | Pair of Pair

    member this.TypeName =
        match this with
        | Nil -> "Value.Nil"
        | Unit -> "Value.Unit"
        | String _ -> "Value.String"
        | Symbol _ -> "Value.Symbol"
        | Keyword _ -> "Value.Keyword"
        | Number _ -> "Value.Number"
        | Char _ -> "Value.Char"
        | Bool _ -> "Value.Bool"
        | Any _ -> "Value.Any"
        | Atom _ -> "Value.Atom"
        | List _ -> "Value.List"
        | Vector _ -> "Value.Vector"
        | HashMap _ -> "Value.HashMap"
        | HashSet _ -> "Value.HashSet"
        | Pair _ -> "Value.Pair"

    override this.GetHashCode() =
        System.HashCode.Combine(
            this.GetType(),
            match this with
            | Vector v -> v.GetHashCode()
            | List v -> v.GetHashCode()
            | String v -> HashCode.Combine("string", v)
            | Symbol v -> HashCode.Combine("symbol", v)
            | Keyword v -> HashCode.Combine("keyword", v)
            | Number v -> v.GetHashCode()
            | Char v -> v.GetHashCode()
            | Bool v -> v.GetHashCode()
            | Atom v -> v.GetHashCode()
            | Any v -> v.GetHashCode()
            | HashMap v -> v.GetHashCode()
            | HashSet v -> v.GetHashCode()
            | Pair v -> v.GetHashCode()
            | Nil -> 999998
            | Unit -> 999999
        )

    override this.Equals other =
        match other with
        | :? Value as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<Value> with
        member this.Equals other =
            match (this, other) with
            | (Nil, Nil) -> true
            | (Unit, Unit) -> true
            | (String lhs, String rhs) -> lhs.Equals(rhs)
            | (Symbol lhs, Symbol rhs) -> lhs.Equals(rhs)
            | (Keyword lhs, Keyword rhs) -> lhs.Equals(rhs)
            | (Number lhs, Number rhs) -> lhs.Equals(rhs)
            | (Char lhs, Char rhs) -> lhs.Equals(rhs)
            | (Bool lhs, Bool rhs) -> lhs.Equals(rhs)
            | (Any lhs, Any rhs) -> lhs.Equals(rhs)
            | (Atom lhs, Atom rhs) -> lhs.Equals(rhs)
            | (List lhs, List rhs) -> lhs.Equals(rhs)
            | (Vector lhs, Vector rhs) -> lhs.Equals(rhs)
            | (HashMap lhs, HashMap rhs) -> lhs.Equals(rhs)
            | (HashSet lhs, HashSet rhs) -> lhs.Equals(rhs)
            | (Pair lhs, Pair rhs) -> lhs.Equals(rhs)
            | _ -> false

    member this.CompareTo(other: Value) =
        (this :> IComparable<_>).CompareTo other

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Value as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<Value> with
        member this.CompareTo other =
            match (this, other) with
            | (Nil, Nil) -> 0
            | (Unit, Unit) -> 0
            | (String lhs, String rhs) -> lhs.CompareTo(rhs)
            | (Symbol lhs, Symbol rhs) -> lhs.CompareTo(rhs)
            | (Keyword lhs, Keyword rhs) -> lhs.CompareTo(rhs)
            | (Number lhs, Number rhs) -> (lhs :> IComparable<_>).CompareTo(rhs)
            | (Char lhs, Char rhs) -> lhs.CompareTo(rhs)
            | (Bool lhs, Bool rhs) -> lhs.CompareTo(rhs)
            | (Any lhs, Any rhs) -> lhs.CompareTo(rhs)
            | (Atom lhs, Atom rhs) -> lhs.CompareTo(rhs)
            | (List lhs, List rhs) -> lhs.CompareTo(rhs)
            | (Vector lhs, Vector rhs) -> lhs.CompareTo(rhs)
            | (HashMap lhs, HashMap rhs) -> lhs.CompareTo(rhs)
            | (HashSet lhs, HashSet rhs) -> lhs.CompareTo(rhs)
            | (Pair lhs, Pair rhs) -> lhs.CompareTo(rhs)
            | (lhs, rhs) -> lhs.TypeName.CompareTo(rhs.TypeName)

    static member list(it: Value seq) = List(ValueList(List.ofSeq it))

    static member number(it: Number) = Number it

    static member vector(it: Value seq) =
        let v = new Vector()
        v.AddRange(it)
        Value.Vector(v)

    static member vector(it: Vector) =
        let v = new Vector(it.Count)
        v.AddRange(it)
        Value.Vector(v)

    static member hashset(it: Value list) = HashSet(HashSet.HashSet(Set.ofList it))

    static member hashmap(it: (Value * Value) list) = HashMap(HashMap.HashMap(Map.ofList it))

    static member atom(it: Value) = Atom(new Atom(it))

    static member bool(it: bool) = Bool it

    static member keyword(it: string) = Keyword it

    static member symbol(it: string) = Symbol it

    static member string(it: string) = String it

    static member from(it: int32) = Number(Number.from it)

    static member from(it: int64) = Number(Number.from it)
    static member int(it: int64) = Number(Number.from it)

    static member from(it: string) = String it

    static member from(it: char) = Char it

    static member from(it: bool) = Bool it

    static member from(it: Vector) = Vector it

    static member from(it: Atom) = Atom it

    static member from(it: HashMap) = HashMap it

    static member from(it: HashSet) = HashSet it

    static member from(it: Value list) = List(ValueList it)

    static member from(it: ValueList) = List it

    static member from(it: decimal) = Number(Number.from it)

    static member from(it: Value) = it

    static member from(it: Object) =
        match it with
        | :? int32 as x -> Value.from x
        | :? decimal as x -> Value.from x
        | :? Vector as x -> Vector x
        | :? ValueList as x -> Value.from x
        | :? string as x -> String x
        | :? char as x -> Char x
        | :? Atom as x -> Atom x
        | :? list<Value> as x -> Value.from (x)
        | _ -> Any(AnyValue it)

    static let printValueList (lst: Value list) (writer: TextWriter) =
        writer.Write '('
        let mutable fst = true

        for item in lst do
            if fst then fst <- false else writer.Write(' ')
            item.writeTo writer

        writer.Write ')'

    static let tryWriteObject (ob: Object) (writer: TextWriter) =
        match ob with
        | :? int32 as x -> writer.Write(x)
        | :? decimal as x -> writer.Write(Impl.toInvariantString x)
        | :? Vector as x -> writer.Write x
        | :? string as x ->
            writer.Write '"'
            writer.Write x
            writer.Write '"'
        | :? char as x -> writer.Write x
        | :? IWriteTo as x -> x.writeTo writer
        | _ -> writer.Write(Impl.toInvariantString ob)

        ()

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            match this with
            | List it -> it.writeTo writer
            | Symbol it -> writer.Write(it)
            | Keyword it -> writer.Write(it)
            | Vector it -> it.writeTo writer
            | HashMap it -> it.writeTo writer
            | HashSet it -> it.writeTo writer
            | Number it -> it.writeTo writer
            | Pair it -> it.writeTo writer
            | Char it -> writer.Write(it)
            | String it ->
                writer.Write('"')
                writer.Write(it)
                writer.Write('"')
            | Unit -> writer.Write("()")
            | Nil -> writer.Write("nil")
            | Bool v -> writer.Write(if v then "true" else "false")
            | Atom it -> it.writeTo writer
            | Any it -> tryWriteObject it writer

            ()



    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

and ValueArray = ResizeArray<Value>

and Atom(init: Value) =
    member val Value = init with get, set

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            writer.Write("atom(")
            this.Value.writeTo (writer)
            writer.Write(")")

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

    override this.GetHashCode() =
        System.HashCode.Combine("Atom", this.Value)

    override this.Equals other =
        match other with
        | :? Vector as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    member this.CompareTo(other: Atom) =
        (this :> IComparable<_>).CompareTo other

    interface IEquatable<Atom> with
        member this.Equals other = this.Value.Equals(other.Value)

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Atom as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<Atom> with
        member this.CompareTo other = this.Value.CompareTo other.Value

and [<Struct; CustomComparison; CustomEquality>] ValueList =
    | ValueList of Value list

    member this.List =
        let (ValueList it) = this
        it

    member this.Length =
        let (ValueList it) = this
        it.Length

    member this.Count =
        let (ValueList it) = this
        it.Length

    member this.GetEnumerator() =
        let (ValueList it) = this
        (it :> IEnumerable<Value>).GetEnumerator()

    override this.GetHashCode() =
        let mutable hash = System.HashCode.Combine(this.Count)

        for item in this.List do
            hash <- System.HashCode.Combine(item, hash)

        hash

    override this.Equals other =
        match other with
        | :? Vector as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<ValueList> with
        member this.Equals other =
            if this.Count <> other.Count then
                false
            else
                Seq.zip this.List other.List |> Seq.forall (fun (a, b) -> a.Equals(b))

    member this.CompareTo(other: ValueList) =
        (this :> IComparable<_>).CompareTo other

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? ValueList as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<ValueList> with
        member this.CompareTo other =
            let this_len = this.Count
            let other_len = other.Count

            if this_len <> other_len then
                this_len.CompareTo(other_len)
            else
                let mutable this_enu = this.GetEnumerator()
                let mutable other_enu = other.GetEnumerator()
                let mutable is_done = false
                let mutable result = 0

                while (not is_done) && (this_enu.MoveNext()) do
                    other_enu.MoveNext() |> ignore

                    result <- this_enu.Current.CompareTo(other_enu.Current)

                    if result <> 0 then
                        is_done <- true

                result

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            writer.Write('(')
            let mutable first = true

            for item in this do
                if first then
                    first <- false
                else
                    writer.Write(' ') |> ignore

                item.writeTo writer

            writer.Write(')')

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

and [<Struct; CustomComparison; CustomEquality>] AnyValue =
    | AnyValue of Object

    member this.Value =
        let (AnyValue ob) = this
        ob

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            let tryWriteObject (ob: Object) (writer: TextWriter) =
                match ob with
                | :? int32 as x -> writer.Write(x)
                | :? decimal as x -> writer.Write(Impl.toInvariantString x)
                | :? Vector as x -> writer.Write x
                | :? string as x ->
                    writer.Write '"'
                    writer.Write x
                    writer.Write '"'
                | :? char as x -> writer.Write x
                | :? IWriteTo as x -> x.writeTo writer
                | _ -> writer.Write(Impl.toInvariantString ob)

                ()

            tryWriteObject this.Value writer

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

    override this.GetHashCode() = this.Value.GetHashCode()

    override this.Equals other =
        match other with
        | :? AnyValue as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<AnyValue> with
        member this.Equals other = this.Value.Equals(other.Value)

    member this.CompareTo(other: AnyValue) =
        (this :> IComparable<_>).CompareTo other

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? AnyValue as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<AnyValue> with
        member this.CompareTo _other = 0

and [<Struct; CustomComparison; CustomEquality>] Pair =
    | Pair of Value * Value

    member this.Car =
        let (Pair(car, _)) = this
        car

    member this.Cdr =
        let (Pair(_, cdr)) = this
        cdr

    override this.GetHashCode() =
        System.HashCode.Combine(this.Car, this.Cdr)

    override this.Equals other =
        match other with
        | :? Pair as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<Pair> with
        member this.Equals other =
            this.Car.Equals(other.Car) && this.Cdr.Equals(other.Cdr)

    member this.CompareTo(other: Pair) =
        (this :> IComparable<_>).CompareTo other

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Pair as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<Pair> with
        member this.CompareTo other =
            let result = this.Car.CompareTo(other.Car)

            if result <> 0 then
                result
            else
                this.Cdr.CompareTo(other.Cdr)

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            writer.Write("(")
            this.Car.writeTo (writer)
            writer.Write(" . ")
            this.Cdr.writeTo (writer)
            writer.Write(')')

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

and [<Struct; CustomComparison; CustomEquality>] HashSet =
    | HashSet of Set<Value>

    member this.Set =
        let (HashSet m) = this
        m

    member this.Count = this.Set.Count

    member this.GetEnumerator() =
        (this.Set :> IEnumerable<Value>).GetEnumerator()

    override this.GetHashCode() =
        let mutable hash = System.HashCode.Combine(this.Count)

        for item in this do
            hash <- System.HashCode.Combine(item, hash)

        hash

    override this.Equals other =
        match other with
        | :? HashSet as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<HashSet> with
        member this.Equals other =
            if this.Count <> other.Count then
                false
            else
                Seq.zip this.Set other.Set |> Seq.forall (fun (a, b) -> a.Equals(b))

    member this.CompareTo(other: HashSet) =
        (this :> IComparable<_>).CompareTo other

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? HashSet as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<HashSet> with
        member this.CompareTo other =
            let this_len = this.Count
            let other_len = other.Count

            if this_len <> other_len then
                this_len.CompareTo(other_len)
            else
                let mutable this_enu = this.GetEnumerator()
                let mutable other_enu = other.GetEnumerator()
                let mutable is_done = false
                let mutable result = 0

                while (not is_done) && (this_enu.MoveNext()) do
                    other_enu.MoveNext() |> ignore

                    result <- this_enu.Current.CompareTo(other_enu.Current)

                    if result <> 0 then
                        is_done <- true

                result

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            writer.Write("#{")
            let mutable first = true

            for item in this do
                if first then
                    first <- false
                else
                    writer.Write(' ') |> ignore

                item.writeTo writer

            writer.Write('}')

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

and [<Struct; CustomComparison; CustomEquality>] HashMap =
    | HashMap of Map<Value, Value>

    member this.Count =
        let (HashMap m) = this
        m.Count

    member this.Map =
        let (HashMap m) = this
        m

    member this.GetEnumerator() =
        let (HashMap m) = this

        (m :> IEnumerable<KeyValuePair<Value, Value>>).GetEnumerator()


    member this.CompareTo(other: HashMap) =
        (this :> IComparable<_>).CompareTo other

    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            writer.Write('{')
            let mutable first = true

            let (HashMap map) = this

            for kvp in map do
                if first then
                    first <- false
                else
                    writer.Write(' ') |> ignore

                kvp.Key.writeTo writer
                writer.Write(' ')
                kvp.Value.writeTo writer

            writer.Write('}')

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

    override this.GetHashCode() =
        let mutable hash = System.HashCode.Combine(this.Count)

        for item in this.Map do
            hash <- System.HashCode.Combine(item, hash)

        hash

    override this.Equals other =
        match other with
        | :? Vector as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<HashMap> with
        member this.Equals other =
            if this.Count <> other.Count then
                false
            else
                Seq.zip this.Map other.Map |> Seq.forall (fun (a, b) -> a.Equals(b))

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? HashMap as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<HashMap> with
        member this.CompareTo other =
            let this_len = this.Count
            let other_len = other.Count

            if this_len <> other_len then
                this_len.CompareTo(other_len)
            else
                let mutable this_enu = this.GetEnumerator()
                let mutable other_enu = other.GetEnumerator()
                let mutable is_done = false
                let mutable result = 0

                while (not is_done) && (this_enu.MoveNext()) do
                    other_enu.MoveNext() |> ignore

                    let lhs = this_enu.Current
                    let rhs = other_enu.Current

                    result <- lhs.Key.CompareTo(rhs.Key)

                    if result <> 0 then
                        is_done <- true
                    else
                        result <- lhs.Value.CompareTo(rhs.Key)

                        if result <> 0 then
                            is_done <- true


                result


and Vector =
    inherit ValueArray

    new() = { inherit ValueArray() }

    new(capacity: int) = { inherit ValueArray(capacity) }

    member this.ToSeq() : seq<Value> = this

    member this.AddRange(span: ReadOnlySpan<Value>) =
        this.EnsureCapacity(this.Capacity + span.Length) |> ignore

        for item in span do
            this.Add(item)

    override this.GetHashCode() =
        let mutable hash = System.HashCode.Combine(this.Count)

        for item in this do
            hash <- System.HashCode.Combine(item, hash)

        hash

    override this.Equals other =
        match other with
        | :? Vector as v -> (this :> IEquatable<_>).Equals v
        | _ -> false

    interface IEquatable<Vector> with
        member this.Equals other =
            if this.Count <> other.Count then
                false
            else
                Seq.zip this other |> Seq.forall (fun (a, b) -> a.Equals(b))

    member this.CompareTo(other: Vector) =
        (this :> IComparable<_>).CompareTo other

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Vector as v -> (this :> IComparable<_>).CompareTo v
            | _ -> -1

    interface IComparable<Vector> with
        member this.CompareTo other =
            let this_len = this.Count
            let other_len = other.Count

            if this_len <> other_len then
                this_len.CompareTo(other_len)
            else
                let mutable this_enu = this.GetEnumerator()
                let mutable other_enu = other.GetEnumerator()
                let mutable is_done = false
                let mutable result = 0

                while (not is_done) && (this_enu.MoveNext()) do
                    other_enu.MoveNext() |> ignore

                    result <- this_enu.Current.CompareTo(other_enu.Current)

                    if result <> 0 then
                        is_done <- true

                result


    member this.writeTo(writer: TextWriter) = (this :> IWriteTo).writeTo (writer)

    interface IWriteTo with
        member this.writeTo(writer: TextWriter) =
            writer.Write('[')
            let mutable first = true

            for item in this do
                if first then
                    first <- false
                else
                    writer.Write(' ') |> ignore

                item.writeTo writer

            writer.Write(']')

    override this.ToString() =
        let sb = PooledStringBuilder.Get()
        use writer = new StringWriter(sb)
        this.writeTo writer
        PooledStringBuilder.ToStringAndReturn(sb)

[<RequireQualifiedAccess>]
module Unwrap =
    let number d =
        match d with
        | Value.Number lst -> lst
        | _ -> failwithf "not supported %O" d

    let int d =
        match number d with
        | Number.Int64 it -> it
        | _ -> failwithf "not supported %O" d

    let int64 d =
        match number d with
        | Number.Int64 it -> it
        | _ -> failwithf "not supported %O" d

    let int32 d =
        match number d with
        | Number.Int64 it -> (int32 it)
        | _ -> failwithf "not supported %O" d

    let i64 = int64
    let i32 = int32

    let hashset d =
        match d with
        | Value.HashSet it -> it
        | _ -> failwithf "not supported %O" d

    let set d =
        let (HashSet it) = hashset d
        it
// match  with
// | HashSet it -> it
// | _ -> failwithf "not supported %O" d

[<AutoOpen>]
module Value =
    let unwrapList d =
        match d with
        | Value.List lst -> lst.List
        | _ -> failwithf "not supported %O" d

    let unwrapVec d =
        match d with
        | Value.Vector lst -> lst
        | _ -> failwithf "not supported %O" d

    let unwrapSeq d : seq<Value> =
        match d with
        | Value.List lst -> lst.List
        | Value.Vector lst -> lst.ToSeq()
        | _ -> failwithf "not supported %O" d

    let unwrapAtom d =
        match d with
        | Value.Atom lst -> lst
        | _ -> failwithf "not supported %O" d

    let unwrapNumber d =
        match d with
        | Value.Number lst -> lst
        | _ -> failwithf "not supported %O" d

    let unwrapInt d =
        match unwrapNumber d with
        | Number.Int64 it -> it
        | _ -> failwithf "not supported %O" d

    let unwrapPair d =
        match d with
        | Value.Pair it -> it
        | _ -> failwithf "not supported %O" d

    let deref d =
        match d with
        | Value.Atom it -> it.Value
        | _ -> failwithf "not supported %O" d

    let pair a b = Value.Pair(Pair(a, b))

    let car (p: Pair) = p.Car

    let cdr (p: Pair) = p.Cdr

    let tuple2 a b = (a, b)

    let vec (items: Value seq) =
        let v = new Vector()
        v.AddRange(items)
        v

    let copyVec (items: Vector) =
        let v = new Vector(items.Count)
        v.AddRange(items)
        v

    let enumerateRange (c: int) = seq { 0 .. (c - 1) }

module HashSet =
    let ofList (values: Value list) = HashSet(Set.ofList values)


module HashMap =
    let ofList (values: (Value * Value) list) = HashMap(Map.ofList values)

    let find value (m: HashMap) = Map.find value m.Map
