using System;
using System.Runtime.CompilerServices;

namespace NativeIntBitwiseNotTest
{
    class Program
    {
        static int Main(string[] args)
        {
            // `not` on a native unsigned int. This is the shape the BCL's unrolled
            // loops use to round a count down to a multiple of a power of two, e.g.
            // SpanHelpers.Fill's `numElements & ~(nuint)7`.
            nuint seven = 7;
            nuint mask = ~seven;

            if ((nuint)23 == (23 & mask))
            {
                return 1;
            }

            if ((23 & mask) != 16)
            {
                return 2;
            }

            if ((8 & mask) != 8)
            {
                return 3;
            }

            if ((7 & mask) != 0)
            {
                return 4;
            }

            // ~0 is all bits set, so masking with it is the identity.
            nuint zero = 0;

            if ((12345 & ~zero) != 12345)
            {
                return 5;
            }

            if (~(nuint)0 == 0)
            {
                return 6;
            }

            // Double complement is the identity.
            if (~~seven != 7)
            {
                return 7;
            }

            // `not` on a signed native int: ~x == -x - 1.
            nint five = 5;

            if (~five != -6)
            {
                return 8;
            }

            nint negSix = -6;

            if (~negSix != 5)
            {
                return 9;
            }

            nint minusOne = -1;

            if (~minusOne != 0)
            {
                return 10;
            }

            // Wider than 32 bits, to confirm the operation is done at native width
            // rather than being truncated to int.
            nint big = (nint)(1L << 40);

            if (~big != (nint)(-(1L << 40) - 1))
            {
                return 11;
            }

            nuint bigU = (nuint)(1UL << 40);

            if ((~bigU & (nuint)(1UL << 40)) != 0)
            {
                return 12;
            }

            if ((~bigU & (nuint)(1UL << 41)) != (nuint)(1UL << 41))
            {
                return 13;
            }

            // `UIntPtr.Zero` reaches the eval stack as a null managed pointer rather than
            // as a verbatim integer, but PawPrint models null as exactly the bit pattern 0,
            // so complementing it is an ordinary all-ones integer and still composes with
            // the masking arms below.
            nuint zeroPtr = UIntPtr.Zero;

            if (~zeroPtr == 0)
            {
                return 14;
            }

            if ((12345 & ~zeroPtr) != 12345)
            {
                return 15;
            }

            if (~~zeroPtr != 0)
            {
                return 16;
            }

            nint zeroIntPtr = IntPtr.Zero;

            if (~zeroIntPtr != -1)
            {
                return 17;
            }

            // Synthesised pointer-hash bits. A `TypeHandle.Value` has no real address, so
            // PawPrint gives it deterministic opaque bits; widening to ulong and narrowing
            // back with `conv.u` is the established way to land those in the native-int
            // slot (see XorNativeIntOpaqueHashBits.cs). `not` must keep them deterministic
            // and stay inside the opaque domain rather than claiming a verbatim value.
            IntPtr handle = typeof(int).TypeHandle.Value;
            ulong widened = (ulong)handle;
            nuint opaque = (nuint)((widened << 32) | (widened >> 32));

            // Double complement is the identity.
            if (~~opaque != opaque)
            {
                return 18;
            }

            // Complementing actually changes the bits.
            if (~opaque == opaque)
            {
                return 19;
            }

            // x ^ ~x is all ones, whatever representation x has.
            if ((opaque ^ ~opaque) != ~(nuint)0)
            {
                return 20;
            }

            // `x & ~x == 0` is deliberately not checked here: `and` between two
            // NativeInt(OpaqueHashBits) operands is a separate unimplemented case
            // ("refusing to do binary operation on ..."), unrelated to `not`. The
            // xor above already pins that the complemented bits are the right ones.

            // A bit-pattern placeholder byref, which is by construction nothing but the
            // raw bits that produced it. `Unsafe.AsRef<T>((void*)bits)` is how the BCL
            // makes one (see MemoryMarshal.GetNonNullPinnableReference); taking its
            // address back out lands those bits in the native-int slot.
            unsafe
            {
                ref int placeholderRef = ref Unsafe.AsRef<int>((void*)7);
                nuint placeholderBits = (nuint)Unsafe.AsPointer(ref placeholderRef);

                if (placeholderBits != 7)
                {
                    return 21;
                }

                if (~placeholderBits != ~(nuint)7)
                {
                    return 22;
                }

                if ((23 & ~placeholderBits) != 16)
                {
                    return 23;
                }
            }

            return 0;
        }
    }
}
