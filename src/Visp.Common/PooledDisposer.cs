// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

namespace Visp.Common;

/// <summary>
/// Automatically call the given function when disposed with the
/// original value. Intended for use with pooled objects so they
/// can be automatically returned to the pool after use.
/// </summary>
/// <typeparam name="T">Type of object contained</typeparam>
/// <param name="value">Value</param>
/// <param name="ret">Function to call when disposed</param>
public readonly struct PooledDisposer<T>(T value, Action<T> ret) : IDisposable
{
    /// <summary>
    /// The value
    /// </summary>
    public readonly T Value { get; } = value;

    public void Dispose()
    {
        ret(Value);
    }
}
