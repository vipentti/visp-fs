// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using CommunityToolkit.HighPerformance.Buffers;

namespace Visp.Common;

public static class Interner
{
    public static string Intern(string s) => StringPool.Shared.GetOrAdd(s);

    public static string Intern(ReadOnlySpan<char> s) => StringPool.Shared.GetOrAdd(s);

    public static string Intern(Span<char> s) => StringPool.Shared.GetOrAdd(s);
}
