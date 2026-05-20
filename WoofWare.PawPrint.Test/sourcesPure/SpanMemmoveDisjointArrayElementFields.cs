using System;
using System.Runtime.InteropServices;

public class Program
{
    // Regression for the undecidable-overlap fail-loud: two byrefs into the
    // same array but distinct element indices, both with a trailing `Field`
    // projection, are guaranteed disjoint. The shared-storage discriminator
    // must include the array index so this copy proceeds rather than
    // tripping the fail-loud diagnostic intended for genuinely-undecidable
    // same-cell overlaps.
    [StructLayout(LayoutKind.Sequential)]
    struct S
    {
        public int A;
        public int B;
        public int C;
    }

    public static int Main(string[] args)
    {
        S[] arr = new S[2];
        arr[0] = new S { A = 11, B = 22, C = 33 };
        arr[1] = new S { A = 44, B = 55, C = 66 };

        Span<int> srcSpan = MemoryMarshal.CreateSpan(ref arr[0].A, 1);
        Span<int> destSpan = MemoryMarshal.CreateSpan(ref arr[1].A, 1);
        srcSpan.CopyTo(destSpan);

        if (arr[0].A != 11) return 10 + arr[0].A;
        if (arr[0].B != 22) return 20 + arr[0].B;
        if (arr[0].C != 33) return 30 + arr[0].C;
        if (arr[1].A != 11) return 40 + arr[1].A;
        if (arr[1].B != 55) return 50 + arr[1].B;
        if (arr[1].C != 66) return 60 + arr[1].C;
        return 0;
    }
}
