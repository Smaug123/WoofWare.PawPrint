using System.Runtime.InteropServices;

public class MarshalSizeOfScalarMarshalAsTest
{
    // [MarshalAs] with a scalar UnmanagedType is a perfectly normal annotation on a
    // matching-width primitive field. The previous Marshal.SizeOf path silently ignored these
    // descriptors and relied on the managed CLI size; the descriptor-aware path must continue
    // to admit them rather than rejecting every non-ByVal descriptor.
    [StructLayout(LayoutKind.Sequential)]
    struct ScalarMatching
    {
        [MarshalAs(UnmanagedType.I4)]
        public int A;
        [MarshalAs(UnmanagedType.U2)]
        public ushort B;
        [MarshalAs(UnmanagedType.I1)]
        public sbyte C;
    }

    public static int Main(string[] argv)
    {
        // A: 4, B: 2 (offset 4), C: 1 (offset 6), tail-pad to alignment 4 -> 8.
        if (Marshal.SizeOf(typeof(ScalarMatching)) != 8) return 1;
        return 0;
    }
}
