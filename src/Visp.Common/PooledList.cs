// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using System.Collections.Immutable;
using Microsoft.Extensions.ObjectPool;

namespace Visp.Common;

public static class PooledList
{
    public static void ReturnToPool<T>(this List<T> obj) => PooledList<T>.Return(obj);

    public static T[] ToArrayAndReturn<T>(this List<T> s) => PooledList<T>.ToArrayAndReturn(s);

    public static ImmutableArray<T> ToImmutableAndReturn<T>(this List<T> s) =>
        PooledList<T>.ToImmutableAndReturn(s);

    public static List<T> Get<T>() => PooledList<T>.Get();

    public static PooledDisposer<List<T>> GetPooled<T>() => new(Get<T>(), ReturnToPool);
}

public static class PooledList<T>
{
    internal class PooledListPolicy : PooledObjectPolicy<List<T>>
    {
        public int InitialCapacity { get; set; } = 100;

        public int MaximumRetainedCapacity { get; set; } = 4096;

        public override List<T> Create() => new(InitialCapacity);

        public override bool Return(List<T> obj)
        {
            if (obj.Capacity > MaximumRetainedCapacity)
            {
                return false;
            }

            obj.Clear();
            return true;
        }
    }

    private static readonly ObjectPool<List<T>> s_pool = new DefaultObjectPool<List<T>>(
        new PooledListPolicy()
    );

    public static List<T> Get() => s_pool.Get();

    public static void Return(List<T> obj)
    {
        s_pool.Return(obj);
    }

    public static T[] ToArrayAndReturn(List<T> s)
    {
        var ret = s.ToArray();
        Return(s);
        return ret;
    }

    public static ImmutableArray<T> ToImmutableAndReturn(List<T> s)
    {
        var ret = ImmutableArray.CreateBuilder<T>(s.Capacity);
        ret.AddRange(s);
        Return(s);
        return ret.MoveToImmutable();
    }
}
