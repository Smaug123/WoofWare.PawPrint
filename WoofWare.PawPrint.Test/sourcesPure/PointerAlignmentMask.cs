using System;
using System.Runtime.InteropServices;

// `(int)ptr & mask` is how CoreLib asks "is this pointer aligned?". The instance that
// matters is `SpanHelpers.IndexOfNullCharacter`, which opens with
// `((int)searchSpace & 1) != 0` and is what `String.wcslen` — and hence
// `new string(char*)` — runs first.
//
// PawPrint models a byref as an unknown container base plus a known in-container
// offset, so it can answer any mask that stays inside the alignment the real runtime
// guarantees for that container. Every assertion below is therefore a fact both
// runtimes agree on, and nothing here depends on an actual machine address:
//
//   * CoreCLR allocates objects 8-byte aligned on x64 and puts SZARRAY data at a
//     16-byte header offset, so `&arr[k]` has low three bits `k * sizeof(T)`.
//   * String character data sits at object + 12 (MethodTable* + length), so it is
//     4-byte aligned — hence masks up to 3, and no further.
//   * `NativeMemory.Alloc` is `malloc`, which is aligned for any fundamental type
//     (16 bytes on x64), so masks up to 7 are safe.
//   * `stackalloc` has no documented alignment beyond the stack's own, so only the
//     2-byte claim implied by the element type is asserted.
unsafe class PointerAlignmentMask
{
    static int TestCharArrayElements()
    {
        char[] chars = new char[8];
        fixed (char* p = chars)
        {
            if (((int)p & 1) != 0)
                return 1;
            if (((int)p & 7) != 0)
                return 2;
            if (((int)(p + 1) & 7) != 2)
                return 3;
            if (((int)(p + 2) & 7) != 4)
                return 4;
            if (((int)(p + 3) & 7) != 6)
                return 5;
            // Wrapping back to the start of the next 8-byte block.
            if (((int)(p + 4) & 7) != 0)
                return 6;
        }

        return 0;
    }

    static int TestByteArrayElements()
    {
        byte[] bytes = new byte[8];
        fixed (byte* p = bytes)
        {
            if (((int)p & 7) != 0)
                return 10;
            if (((int)(p + 3) & 7) != 3)
                return 11;
            if (((int)(p + 5) & 3) != 1)
                return 12;
        }

        return 0;
    }

    static int TestIntArrayElements()
    {
        int[] ints = new int[4];
        fixed (int* p = ints)
        {
            if (((int)p & 7) != 0)
                return 20;
            if (((int)(p + 1) & 7) != 4)
                return 21;
            if (((int)(p + 2) & 7) != 0)
                return 22;
        }

        return 0;
    }

    static int TestStringCharacters()
    {
        string s = "hello";
        fixed (char* p = s)
        {
            // String data is only 4-byte aligned, so 3 is the widest honest mask.
            if (((int)p & 3) != 0)
                return 30;
            if (((int)(p + 1) & 3) != 2)
                return 31;
            if (((int)(p + 2) & 3) != 0)
                return 32;
        }

        return 0;
    }

    static int TestNativeMemory()
    {
        void* block = NativeMemory.Alloc(32);

        try
        {
            byte* p = (byte*)block;
            if (((int)p & 7) != 0)
                return 40;
            if (((int)(p + 1) & 7) != 1)
                return 41;
            if (((int)(p + 6) & 3) != 2)
                return 42;
        }
        finally
        {
            NativeMemory.Free (block);
        }

        return 0;
    }

    static int TestStackalloc()
    {
        char* p = stackalloc char[4];
        if (((int)p & 1) != 0)
            return 50;
        if (((int)(p + 1) & 1) != 0)
            return 51;

        return 0;
    }

    // The mask that motivated all of this: `unchecked` truncation to int followed by
    // a one-bit alignment test, exactly as `IndexOfNullCharacter` writes it.
    static int TestAlignmentPredicateShape()
    {
        char[] chars = new char[2];
        fixed (char* p = chars)
        {
            bool misaligned = ((int)p & 1) != 0;
            if (misaligned)
                return 60;

            // The complementary shape: masking with an all-ones mask leaves the
            // pointer's own bits untouched, so the predicate is still false.
            if ((((int)p & -1) & 1) != 0)
                return 61;
        }

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestCharArrayElements();
        if (result != 0)
            return result;

        result = TestByteArrayElements();
        if (result != 0)
            return result;

        result = TestIntArrayElements();
        if (result != 0)
            return result;

        result = TestStringCharacters();
        if (result != 0)
            return result;

        result = TestNativeMemory();
        if (result != 0)
            return result;

        result = TestStackalloc();
        if (result != 0)
            return result;

        result = TestAlignmentPredicateShape();
        if (result != 0)
            return result;

        return 0;
    }
}
