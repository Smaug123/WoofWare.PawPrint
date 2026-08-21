using System.Runtime.Intrinsics;

internal static class Program
{
    private static int Main()
    {
        // Vector{64,128,256,512}<T>.IsSupported asks whether T is a valid vector
        // element type. It is independent of hardware acceleration: real .NET
        // answers true for the twelve primitive element types even on hardware
        // with no SIMD at all, and ThrowHelper.ThrowForUnsupportedIntrinsicsVectorNNNBaseType
        // relies on that to no-op on live scalar paths.
        if (!Vector64<byte>.IsSupported)
        {
            return 1;
        }

        if (!Vector128<ulong>.IsSupported)
        {
            return 2;
        }

        if (!Vector256<int>.IsSupported)
        {
            return 3;
        }

        if (!Vector512<double>.IsSupported)
        {
            return 4;
        }

        if (!Vector256<nuint>.IsSupported)
        {
            return 5;
        }

        // Non-numeric element types answer false at every width.
        if (Vector64<char>.IsSupported)
        {
            return 6;
        }

        if (Vector128<bool>.IsSupported)
        {
            return 7;
        }

        if (Vector256<char>.IsSupported)
        {
            return 8;
        }

        if (Vector512<bool>.IsSupported)
        {
            return 9;
        }

        return 0;
    }
}
