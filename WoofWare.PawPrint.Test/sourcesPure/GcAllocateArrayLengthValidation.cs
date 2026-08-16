using System;

// The two length checks `GCInterface_AllocateNewArray` inherits from `AllocateSzArray`
// (gchelpers.cpp:624-641), and the exceptions they raise.
//
// Both are checked before any allocation is attempted, so the over-long cases below cost nothing
// on either runtime. Nothing here allocates at or just below `Array.MaxLength`: that would be a
// real multi-gigabyte allocation on the oracle side.
public class TestGcAllocateArrayLengthValidation
{
    // `Array.MaxLength`, which upstream keeps equal to the native `MaxArrayLength()`.
    private const int MaxLength = 0x7FFFFFC7;

    public static int Main(string[] argv)
    {
        // `AllocateArray<T>` has no managed shortcut, so a negative length reaches the QCall
        // whatever T is.
        try
        {
            GC.AllocateArray<int>(-1);
            return 1;
        }
        catch (OverflowException)
        {
        }

        // A release-built CoreLib short-circuits an *unpinned*
        // `AllocateUninitializedArray<T>` to `new T[length]` whenever `length < 2048 / sizeof(T)`
        // (GC.CoreCLR.cs:806-812), and that comparison is signed — so it is trivially true for a
        // negative length, and the unpinned spelling would never reach the QCall at all. Passing
        // `pinned: true` skips the whole `if (!pinned)` block, and incidentally exercises flags
        // 80 (GC_ALLOC_ZEROING_OPTIONAL | GC_ALLOC_PINNED_OBJECT_HEAP).
        try
        {
            GC.AllocateUninitializedArray<byte>(-1, true);
            return 2;
        }
        catch (OverflowException)
        {
        }

        // Past `MaxArrayLength()`: OutOfMemoryException, carrying the native resource string
        // IDS_EE_ARRAY_DIMENSIONS_EXCEEDED rather than the parameterless ctor's default. The
        // literal is safe to assert across runtimes: CoreCLR's native (mscorrc) strings ship
        // English-only in .NET Core, and both runtimes run this same guest text.
        try
        {
            GC.AllocateArray<int>(int.MaxValue);
            return 3;
        }
        catch (OutOfMemoryException e)
        {
            if (e.Message != "Array dimensions exceeded supported range.") return 4;

            // And it is not the default message, which is what makes the check above
            // more than a tautology on either runtime.
            if (e.Message == new OutOfMemoryException().Message) return 5;
        }

        // The boundary itself: MaxLength + 1 is rejected. (MaxLength is not tested from below.)
        try
        {
            GC.AllocateArray<byte>(MaxLength + 1);
            return 6;
        }
        catch (OutOfMemoryException)
        {
        }

        try
        {
            GC.AllocateUninitializedArray<byte>(MaxLength + 1);
            return 7;
        }
        catch (OutOfMemoryException)
        {
        }

        // A rejected allocation must leave the allocator usable. (The `ObjectHandleOnStack` that
        // both throwing paths leave untouched is a local inside CoreLib, so a guest cannot
        // observe it directly; what it can observe is that the next allocation still works and
        // still returns a correctly shaped array.)
        int[] ok = GC.AllocateArray<int>(3);
        if (ok == null) return 8;
        if (ok.Length != 3) return 9;
        if (ok[0] != 0 || ok[1] != 0 || ok[2] != 0) return 10;
        ok[2] = 42;
        if (ok[2] != 42) return 11;

        return 0;
    }
}
