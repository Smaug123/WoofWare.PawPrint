using System.Runtime.InteropServices;

public class MarshalSizeOfByValTStrTest
{
    // Two structs that differ only in CharSet. Under correct ByValTStr sizing the Ansi
    // variant uses 1 byte per char and the Unicode variant uses 2, so the *byte* size
    // of the Name field differs. The fixed Id (4) and Value (8) fields surround it,
    // and SizeConst=16 is chosen so neither size hits a packing boundary that would
    // mask a per-char miscount.
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    struct AnsiStruct
    {
        public int Id;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 16)]
        public string Name;
        public double Value;
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct UnicodeStruct
    {
        public int Id;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 16)]
        public string Name;
        public double Value;
    }

    public static int Main(string[] argv)
    {
        int ansiSize = Marshal.SizeOf(typeof(AnsiStruct));
        int unicodeSize = Marshal.SizeOf(typeof(UnicodeStruct));

        // 4 (Id) + 16 * 1 (Name) padded to 8 + 8 (Value) = 32
        if (ansiSize != 32) return 1;
        // 4 (Id) + 16 * 2 (Name) padded to 8 + 8 (Value) = 48
        if (unicodeSize != 48) return 2;

        return 0;
    }
}
