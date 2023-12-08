// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using System.Text;
using Microsoft.Extensions.ObjectPool;

namespace Visp.Common;

public static class PooledStringBuilder
{
    private static readonly ObjectPool<StringBuilder> s_pool = new DefaultObjectPool<StringBuilder>(
        new StringBuilderPooledObjectPolicy()
    );

    public static StringBuilder Get() => s_pool.Get();

    public static void ReturnToPool(this StringBuilder s)
    {
        s_pool.Return(s);
    }

    public static string ToStringAndClear(this StringBuilder s)
    {
        var ret = s.ToString();
        s.Clear();
        return ret;
    }

    public static string ToStringAndReturn(this StringBuilder s)
    {
        var ret = s.ToString();
        s_pool.Return(s);
        return ret;
    }
}
