// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using Microsoft.Extensions.ObjectPool;

namespace Visp.Common;

public static class PooledDictionary
{
    public static void ReturnToPool<K, V>(this Dictionary<K, V> it)
        where K : notnull => PooledDictionary<K, V>.Return(it);

    public static Dictionary<K, V> Get<K, V>()
        where K : notnull => PooledDictionary<K, V>.Get();

    public static PooledDisposer<Dictionary<K, V>> GetPooled<K, V>()
        where K : notnull => new(PooledDictionary<K, V>.Get(), ReturnToPool);
}

public static class PooledDictionary<K, V>
    where K : notnull
{
    internal class PooledDictionaryPolicy : PooledObjectPolicy<Dictionary<K, V>>
    {
        public int InitialCapacity { get; set; } = 100;

        public int MaximumAllowedItemCount { get; set; } = 2048;

        public override Dictionary<K, V> Create() => new(InitialCapacity);

        public override bool Return(Dictionary<K, V> obj)
        {
            if (obj.Count > MaximumAllowedItemCount)
            {
                return false;
            }

            obj.Clear();
            return true;
        }
    }

    private static readonly ObjectPool<Dictionary<K, V>> s_pool = new DefaultObjectPool<
        Dictionary<K, V>
    >(new PooledDictionaryPolicy());

    public static Dictionary<K, V> Get() => s_pool.Get();

    public static void Return(Dictionary<K, V> obj)
    {
        s_pool.Return(obj);
    }
}
