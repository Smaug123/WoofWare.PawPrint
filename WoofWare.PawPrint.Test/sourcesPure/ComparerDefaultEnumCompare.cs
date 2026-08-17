using System.Collections.Generic;

// Calling the comparer `Comparer<TEnum>.Default` returns. Which comparer that *selection* picks is
// the sibling `ComparerDefault.cs`'s subject; here `EnumComparer<T>.Compare` delegates to
// `RuntimeHelpers.EnumCompareTo<T>`, an [Intrinsic] PawPrint runs as IL via the `safeIntrinsics`
// allowlist rather than servicing itself.
//
// One enum per underlying type, because the IL that entry admits bottoms out in
// `Enum.CompareTo(object)`, which switches on the runtime element type and calls that primitive's
// own `CompareTo` — so the underlying type is the axis along which the comparison can go wrong, and
// an int-backed enum alone pins only one of the eight arms a C# enum can reach.
//
// Chosen so a *misread* width or signedness inverts an answer rather than merely perturbing it:
// every signed enum straddles zero and every unsigned one has a member above its signed range, so
// reading either as the other flips the comparison. `EL` uses the extremes, which also inverts
// under a 64-to-32-bit truncation and overflows any comparator implemented by subtraction. `EI`'s
// second pair is ordered the opposite way to its first and inverts if truncated to 16 bits.

namespace ComparerDefaultEnumCompareTest
{
    enum ESByte : sbyte { Lo = -5, Hi = 5 }
    enum EByte : byte { Lo = 1, Hi = 200 }
    enum EShort : short { Lo = -300, Hi = 300 }
    enum EUShort : ushort { Lo = 1, Hi = 60000 }
    enum EInt : int { Lo = -7, Hi = 7, TruncLo = 1, TruncHi = 65536 }
    enum EUInt : uint { Lo = 1, Hi = 4000000000 }
    enum ELong : long { Lo = -9223372036854775808L, Hi = 9223372036854775807L }
    enum EULong : ulong { Lo = 1, Hi = 18000000000000000000UL }

    class Program
    {
        // Returns 0 if the comparer orders `lo` strictly before `hi`, or `baseCode`, `baseCode + 1`
        // or `baseCode + 2` naming which of the three relations it got wrong. Asserting all three
        // means a comparer that answered a constant, or one with its sign flipped, still fails.
        static int Check<T>(T lo, T hi, int baseCode)
        {
            Comparer<T> comparer = Comparer<T>.Default;
            if (comparer.Compare(lo, hi) >= 0) return baseCode;
            if (comparer.Compare(hi, lo) <= 0) return baseCode + 1;
            if (comparer.Compare(lo, lo) != 0) return baseCode + 2;
            return 0;
        }

        static int Main(string[] args)
        {
            int r;
            r = Check(ESByte.Lo, ESByte.Hi, 10); if (r != 0) return r;
            r = Check(EByte.Lo, EByte.Hi, 20); if (r != 0) return r;
            r = Check(EShort.Lo, EShort.Hi, 30); if (r != 0) return r;
            r = Check(EUShort.Lo, EUShort.Hi, 40); if (r != 0) return r;
            r = Check(EInt.Lo, EInt.Hi, 50); if (r != 0) return r;
            r = Check(EInt.TruncLo, EInt.TruncHi, 60); if (r != 0) return r;
            r = Check(EUInt.Lo, EUInt.Hi, 70); if (r != 0) return r;
            r = Check(ELong.Lo, ELong.Hi, 80); if (r != 0) return r;
            r = Check(EULong.Lo, EULong.Hi, 90); if (r != 0) return r;
            return 0;
        }
    }
}
