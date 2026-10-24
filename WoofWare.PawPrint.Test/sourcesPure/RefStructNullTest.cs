using System;
using System.Runtime.CompilerServices;

namespace RefStructNullTest
{
    public ref struct Held
    {
        public int X;
    }

    // Roslyn compiles a null test of a value whose type parameter `allows ref struct` to
    // `box !!T` and then `brtrue` or `brfalse`, in Debug and Release alike. A byref-like type
    // cannot be boxed, but the JIT folds the `box` into the branch, as it does for every type: a
    // box is never null (`impBoxPatternMatch`, importer.cpp).
    public static class Tests
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static bool EqualsNull<T>(T x) where T : allows ref struct => x == null;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static bool NotEqualsNull<T>(T x) where T : allows ref struct => x != null;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static int IsNull<T>(T x) where T : allows ref struct
        {
            if (x is null) return 1;
            return 2;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static int IsNotNull<T>(T x) where T : allows ref struct
        {
            if (x is not null) return 3;
            return 4;
        }
    }

    public class Program
    {
        public static int Main(string[] args)
        {
            var held = new Held { X = 5 };
            Span<int> span = stackalloc int[2];

            if (Tests.EqualsNull(held)) return 1;
            if (!Tests.NotEqualsNull(held)) return 2;
            if (Tests.IsNull(held) != 2) return 3;
            if (Tests.IsNotNull(held) != 3) return 4;

            if (Tests.EqualsNull(span)) return 5;
            if (!Tests.NotEqualsNull(span)) return 6;
            if (Tests.IsNull(span) != 2) return 7;
            if (Tests.IsNotNull(span) != 3) return 8;

            // Controls: an ordinary value type, and a reference type, whose box is a no-op, both
            // null and not. Code over `string` is shared, and the JIT folds the branch there too.
            if (Tests.EqualsNull(7)) return 9;
            if (Tests.IsNotNull(7) != 3) return 10;
            if (Tests.EqualsNull("x")) return 11;
            if (!Tests.EqualsNull<string>(null)) return 12;
            if (Tests.IsNull<string>(null) != 1) return 13;
            if (Tests.IsNotNull<string>(null) != 4) return 14;

            // The value survives being tested.
            if (held.X != 5) return 15;

            return 0;
        }
    }
}
