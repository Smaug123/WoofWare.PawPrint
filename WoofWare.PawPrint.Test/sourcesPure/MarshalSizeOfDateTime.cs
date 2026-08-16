using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct WithDateTime
    {
        public int Id;
        public DateTime When;
    }

    public static int Main(string[] args)
    {
        // CoreCLR's `MarshalInfo::MarshalInfo` special-cases `System.DateTime` as
        // `MARSHAL_TYPE_DATE` (8 bytes) before the layout check, so a sequential
        // struct with an `int` followed by a `DateTime` sizes as
        // 4 (int) + 4 (pad to align DateTime at 8) + 8 = 16 bytes.
        // Calling `Marshal.SizeOf(typeof(DateTime))` directly throws on CoreCLR
        // — the `MARSHAL_TYPE_DATE` shortcut only applies when DateTime appears
        // as a field of another marshalable struct.
        //
        // PawPrint's marshal-size walk currently trips its top-level AutoLayout
        // rejection when it recurses into the `DateTime` field, because the BCL
        // declares `System.DateTime` with `LayoutKind.Auto`. Recognising
        // `DateTime` as a host-known wrapper (likely via `BaseClassTypes`) would
        // let the field walk short-circuit with an 8-byte result while
        // continuing to reject top-level `Marshal.SizeOf<DateTime>()`.
        if (Marshal.SizeOf(typeof(WithDateTime)) != 16) return 1;
        return 0;
    }
}
