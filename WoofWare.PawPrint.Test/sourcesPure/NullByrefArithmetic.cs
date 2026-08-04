using System;
using System.Runtime.CompilerServices;

namespace NullByrefArithmetic
{
    // The null byref is the bit pattern 0, so offsetting it is ordinary bit
    // arithmetic and must agree with offsetting any other bit-pattern byref.
    // The results must not be dereferenced, but their byte distances are
    // well-defined and are what the real runtime reports.
    public class Program
    {
        public static int Main(string[] args)
        {
            ref byte nullRef = ref Unsafe.NullRef<byte>();
            if (!Unsafe.IsNullRef(ref nullRef)) return 1;

            ref byte offset = ref Unsafe.Add(ref nullRef, 8);
            if (Unsafe.IsNullRef(ref offset)) return 2;

            // Distance from null recovers the offset.
            if ((long)Unsafe.ByteOffset(ref nullRef, ref offset) != 8) return 3;

            // Landing back on zero must normalise to a null ref again, or
            // IsNullRef would disagree with the CLR's bit-pattern definition.
            ref byte backToNull = ref Unsafe.Add(ref offset, -8);
            if (!Unsafe.IsNullRef(ref backToNull)) return 4;

            // Negative offsets are just as legal on a bit pattern.
            ref byte negative = ref Unsafe.Add(ref nullRef, -4);
            if (Unsafe.IsNullRef(ref negative)) return 5;
            if ((long)Unsafe.ByteOffset(ref nullRef, ref negative) != -4) return 6;

            // A wider element type scales the offset by its size.
            ref int nullInt = ref Unsafe.NullRef<int>();
            ref int intOffset = ref Unsafe.Add(ref nullInt, 3);
            if ((long)Unsafe.ByteOffset(ref nullInt, ref intOffset) != 12) return 7;

            // Byte-offset arithmetic reaches the same place as element
            // arithmetic, so the two byrefs must compare equal.
            ref byte viaBytes = ref Unsafe.AddByteOffset(ref nullRef, (IntPtr)8);
            if (!Unsafe.AreSame(ref viaBytes, ref offset)) return 8;

            return 0;
        }
    }
}
