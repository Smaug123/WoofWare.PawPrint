// `Unsafe.As<TFrom, TTo>(ref TFrom)` on a null byref does not throw: its CoreLib body is
// replaced by `ldarg.0; ret`, and the JIT expansion (NI_SRCS_UNSAFE_As) inserts no null
// check. Reinterpreting a null reference is address-preserving, so null goes in and null
// comes out.
//
// This pins the byref overload rather than `Unsafe.As<T>(object)`: pushing the reinterpret
// through a projection rejects the null managed pointer.

using System.Runtime.CompilerServices;

public class Program
{
    public static int Main(string[] args)
    {
        // Differing TFrom/TTo: the reinterpreting path.
        ref int widened = ref Unsafe.As<byte, int>(ref Unsafe.NullRef<byte>());
        if (!Unsafe.IsNullRef(ref widened))
        {
            return 1;
        }

        // Identical TFrom/TTo: the no-op path.
        ref byte same = ref Unsafe.As<byte, byte>(ref Unsafe.NullRef<byte>());
        if (!Unsafe.IsNullRef(ref same))
        {
            return 2;
        }

        // Reinterpreting twice must stay null rather than accumulating projections.
        ref long twice = ref Unsafe.As<int, long>(ref Unsafe.As<byte, int>(ref Unsafe.NullRef<byte>()));
        if (!Unsafe.IsNullRef(ref twice))
        {
            return 3;
        }

        // A non-null byref must still reinterpret normally.
        int value = 0x2A;
        ref byte firstByte = ref Unsafe.As<int, byte>(ref value);
        if (Unsafe.IsNullRef(ref firstByte))
        {
            return 4;
        }

        return 0;
    }
}
