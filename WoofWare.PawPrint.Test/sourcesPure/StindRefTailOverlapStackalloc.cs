using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace StindRefTailOverlapStackalloc
{
    // A `stind.ref` into stack or native memory whose eight bytes start on no
    // typed value but run into the first half of a `long` written at offset 4.
    // The store touches bytes 0..7 only, so bytes 8..11 must still hold the
    // upper half of the long, and bytes 12..15 must still be zero.
    //
    // Real .NET stores eight bytes and nothing else. PawPrint stores the
    // reference as a typed cell and evicts the `long` cell it overlaps; the
    // eviction must keep the bytes of that cell which the store does not cover.
    //
    // `Unsafe.AsRef<object>(void*)` is how a managed reference gets a stack or
    // native address in C# at all. The reference is never read back; only the
    // bytes beside it are.
    public class Program
    {
        static unsafe int Check(byte* p)
        {
            *(long*)(p + 4) = 0x1122334455667788L;

            ref object slot = ref Unsafe.AsRef<object>(p);
            slot = new object();

            if (*(int*)(p + 8) != 0x11223344) return 1;
            if (*(int*)(p + 12) != 0) return 2;

            return 0;
        }

        public static unsafe int Main(string[] args)
        {
            byte* stack = stackalloc byte[16];
            int stackResult = Check(stack);
            if (stackResult != 0) return stackResult;

            byte* native = (byte*)NativeMemory.AllocZeroed(16);
            try
            {
                int nativeResult = Check(native);
                if (nativeResult != 0) return 10 + nativeResult;
            }
            finally
            {
                NativeMemory.Free(native);
            }

            return 0;
        }
    }
}
