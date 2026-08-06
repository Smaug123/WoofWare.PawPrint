using System;

// A 39-char input with 7 separators: long enough that the real runtime's separator search
// takes `MakeSeparatorListVectorized`, rather than the scalar loop a 5-char string like
// "a/b/c" takes.
//
// PawPrint itself does *not* vectorize this. Every machine state is created with
// `HardwareIntrinsicsProfile.ScalarOnly` (IlMachineThreadState.fs), so
// `Vector128.IsHardwareAccelerated` is false in the guest and CoreLib picks the scalar
// path whatever the input length. That asymmetry is the point of this case rather than a
// defect in it: `sourcesPure` runs the same source under both runtimes and requires the
// exit codes to agree, so this asserts that PawPrint's scalar result matches the answer
// the real runtime reaches through its SIMD path.
//
// It is therefore *not* coverage of `MakeSeparatorListVectorized`'s intrinsics under
// PawPrint, and cannot become so until an accelerated virtual profile exists.
public class TestStringSplitLongInput
{
    public static int Main(string[] argv)
    {
        string s = "aaaa/bbbb/cccc/dddd/eeee/ffff/gggg/hhhh";
        string[] parts = s.Split('/');

        if (parts.Length != 8) return 1;
        if (parts[0] != "aaaa") return 2;
        if (parts[7] != "hhhh") return 3;

        return 0;
    }
}
