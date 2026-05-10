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

    // HRESULT is marshalled as a 4-byte integer just like UnmanagedType.I4. CoreCLR accepts
    // it on `int`/`uint` fields, so we should too.
    [StructLayout(LayoutKind.Sequential)]
    struct WithHresult
    {
        [MarshalAs(UnmanagedType.Error)]
        public int Hr;
        public int Other;
    }

    public static int Main(string[] argv)
    {
        // A: 4, B: 2 (offset 4), C: 1 (offset 6), tail-pad to alignment 4 -> 8.
        if (Marshal.SizeOf(typeof(ScalarMatching)) != 8) return 1;
        // Hr: 4, Other: 4 -> 8.
        if (Marshal.SizeOf(typeof(WithHresult)) != 8) return 2;
        return 0;
    }
}
