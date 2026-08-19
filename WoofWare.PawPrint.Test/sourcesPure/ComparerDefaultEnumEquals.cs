using System.Collections.Generic;

// Calling the equality comparer `EqualityComparer<TEnum>.Default` returns. Which comparer that
// *selection* picks is the sibling `ComparerDefault.cs`'s subject; here
// `EnumEqualityComparer<T>.Equals` delegates to `RuntimeHelpers.EnumEquals<T>`, an [Intrinsic]
// PawPrint runs as IL via the `safeIntrinsics` allowlist rather than servicing itself.
//
// One enum per underlying type, because the IL that entry admits bottoms out in
// `Enum.Equals(object)`, which switches on the runtime element type and compares the two boxes'
// raw bytes at that type's *width*. Width is the whole axis here, and it is the one an int-backed
// enum alone cannot pin: signedness does not enter, since that switch folds I1 with U1, I2 with U2
// and so on, comparing the same bytes either way.
//
// So each type gets a pair of members that agree on their low byte and differ only further up,
// which a too-narrow read reports as equal: 257 against 1 catches a byte-wide read, 65537 against 1
// a 16-bit one, and 2^32+1 against 1 a 32-bit one. Those are the pairs an enum whose members merely
// differ cannot stand in for — under a 16-bit read, 1 and 7 are still distinct.
//
// A read *wider* than the underlying type needs no pair of its own: it runs off the end of the box,
// which the interpreter refuses outright rather than answering from adjacent bytes.

namespace ComparerDefaultEnumEqualsTest
{
    enum ESByte : sbyte { Lo = -5, Hi = 5 }
    enum EByte : byte { Lo = 1, Hi = 200 }
    enum EShort : short { Lo = 1, Hi = 257, Neg = -300 }
    enum EUShort : ushort { Lo = 1, Hi = 257, Big = 60000 }
    enum EInt : int { Lo = 1, Byte = 257, Word = 65537, Neg = -7 }
    enum EUInt : uint { Lo = 1, Word = 65537, Big = 4000000000 }
    enum ELong : long { Lo = 1, Dword = 4294967297L, Min = -9223372036854775808L }
    enum EULong : ulong { Lo = 1, Dword = 4294967297UL, Big = 18000000000000000000UL }

    class Program
    {
        // Returns 0 if the comparer holds `a` equal to itself, `b` equal to itself and the two
        // distinct, or `baseCode`, `baseCode + 1` or `baseCode + 2` naming which it got wrong.
        // Asserting all three means a comparer answering a constant fails whichever constant it is.
        static int Check<T>(T a, T b, int baseCode)
        {
            EqualityComparer<T> comparer = EqualityComparer<T>.Default;
            if (!comparer.Equals(a, a)) return baseCode;
            if (!comparer.Equals(b, b)) return baseCode + 1;
            if (comparer.Equals(a, b)) return baseCode + 2;
            return 0;
        }

        static int Main(string[] args)
        {
            int r;
            r = Check(ESByte.Lo, ESByte.Hi, 10); if (r != 0) return r;
            r = Check(EByte.Lo, EByte.Hi, 20); if (r != 0) return r;
            r = Check(EShort.Lo, EShort.Hi, 30); if (r != 0) return r;
            r = Check(EShort.Lo, EShort.Neg, 40); if (r != 0) return r;
            r = Check(EUShort.Lo, EUShort.Hi, 50); if (r != 0) return r;
            r = Check(EUShort.Lo, EUShort.Big, 60); if (r != 0) return r;
            r = Check(EInt.Lo, EInt.Byte, 70); if (r != 0) return r;
            r = Check(EInt.Lo, EInt.Word, 80); if (r != 0) return r;
            r = Check(EInt.Lo, EInt.Neg, 90); if (r != 0) return r;
            r = Check(EUInt.Lo, EUInt.Word, 100); if (r != 0) return r;
            r = Check(EUInt.Lo, EUInt.Big, 110); if (r != 0) return r;
            r = Check(ELong.Lo, ELong.Dword, 120); if (r != 0) return r;
            r = Check(ELong.Lo, ELong.Min, 130); if (r != 0) return r;
            r = Check(EULong.Lo, EULong.Dword, 140); if (r != 0) return r;
            r = Check(EULong.Lo, EULong.Big, 150); if (r != 0) return r;
            return 0;
        }
    }
}
