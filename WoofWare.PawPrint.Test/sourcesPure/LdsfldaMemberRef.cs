using System;

// Repro for issue #723: `ldsflda` whose operand is a MemberReference token.
public static class LdsfldaMemberRef
{
    private sealed class Holder<T>
    {
        public static int Value;
    }

    public static int Main (string[] args)
    {
        // Static field of a generic type: the token is a MemberRef with a
        // TypeSpec parent even though the type is in this assembly.
        ref int slot = ref Holder<int>.Value;
        slot = 7;
        if (Holder<int>.Value != 7)
        {
            return 1;
        }

        ref int otherSlot = ref Holder<string>.Value;
        otherSlot = 9;
        if (Holder<int>.Value != 7 || Holder<string>.Value != 9)
        {
            return 2;
        }

        // Static field defined in another assembly: the token is a MemberRef
        // with a TypeRef parent.
        ref readonly TimeSpan zero = ref TimeSpan.Zero;
        if (zero.Ticks != 0)
        {
            return 3;
        }

        ref readonly string empty = ref string.Empty;
        if (!ReferenceEquals (empty, string.Empty) || empty.Length != 0)
        {
            return 4;
        }

        return 0;
    }
}
