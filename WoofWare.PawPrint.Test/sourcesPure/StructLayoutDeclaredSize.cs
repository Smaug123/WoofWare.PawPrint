using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A declared `[StructLayout(Size = N)]` is a *floor* on the instance size, and it suppresses the
// alignment rounding that a type without one gets. CoreCLR picks exactly one of the two:
//
//     managedSize = classSizeInMetadata != 0
//                 ? max(classSizeInMetadata + parentSize, lastFieldEnd)   // CalculateSizeWithMetadataSize
//                 : AlignSize(lastFieldEnd, alignmentRequirement)         // classlayoutinfo.cpp:543-550
//
// so a declared `Size` never rounds, and one that is too small loses to the fields rather than
// truncating them.
//
// The one rounding that does survive a declared `Size` is the GC one: a value type containing
// object references is sized to a multiple of the pointer size, because the GC requires it
// (`ValidateExplicitLayout`, methodtablebuilder.cpp:9104). That is why `E9Obj` is 16 while
// `E9Long`, identical but for holding a `long` instead of a reference, is 9.
//
// Types that reach *auto* layout ignore a declared `Size` completely, whether they were declared
// `Auto` or promoted there for holding references, because auto layout never reads the
// `ClassLayout` row at all.
public class TestStructLayoutDeclaredSize
{
    [StructLayout(LayoutKind.Sequential, Size = 13)] public struct S13 { public long A; public int B; }
    [StructLayout(LayoutKind.Sequential)] public struct S0 { public long A; public int B; }
    [StructLayout(LayoutKind.Sequential, Size = 4)] public struct SSmall { public long A; public int B; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] public struct SPack { public byte A; public long B; }

    [StructLayout(LayoutKind.Explicit, Size = 9)] public struct E9Obj { [FieldOffset(0)] public object O; }
    [StructLayout(LayoutKind.Explicit, Size = 9)] public struct E9Long { [FieldOffset(0)] public long L; }
    [StructLayout(LayoutKind.Explicit, Size = 17)] public struct E17Obj { [FieldOffset(0)] public object O; }
    [StructLayout(LayoutKind.Explicit, Size = 3)] public struct E3Obj { [FieldOffset(0)] public object O; }
    [StructLayout(LayoutKind.Explicit, Size = 2)] public struct E2Long { [FieldOffset(0)] public long L; }
    [StructLayout(LayoutKind.Explicit)] public struct EObj { [FieldOffset(0)] public object O; [FieldOffset(8)] public byte B; }

    // Reaches auto layout by GC promotion, so the declared Size is discarded.
    [StructLayout(LayoutKind.Sequential, Size = 13)] public struct SeqObj { public object O; public int I; }
    // Declared auto, so the Size is discarded without any promotion being involved.
    [StructLayout(LayoutKind.Auto, Size = 64)] public struct AutoSized { public int I; }

    // A declared Size leaves the *alignment* requirement alone, so a container still places an
    // `S13` on an 8-byte boundary even though it is 13 bytes wide.
    [StructLayout(LayoutKind.Sequential)] public struct ContainsS13 { public byte B; public S13 V; }

    // Blittable shapes for the marshalling half of the rule, which cannot use the reference-
    // bearing types above.
    [StructLayout(LayoutKind.Sequential, Size = 3)] public struct MByte3 { public byte A; }
    [StructLayout(LayoutKind.Explicit, Size = 13)] public struct ME13 { [FieldOffset(0)] public long A; [FieldOffset(8)] public int B; }

    public static int Main(string[] argv)
    {
        if (Unsafe.SizeOf<S13>() != 13) return 1;
        if (Unsafe.SizeOf<S0>() != 16) return 2;
        if (Unsafe.SizeOf<SSmall>() != 12) return 3;
        if (Unsafe.SizeOf<SPack>() != 9) return 4;

        if (Unsafe.SizeOf<E9Obj>() != 16) return 5;
        if (Unsafe.SizeOf<E9Long>() != 9) return 6;
        if (Unsafe.SizeOf<E17Obj>() != 24) return 7;
        if (Unsafe.SizeOf<E3Obj>() != 8) return 8;
        if (Unsafe.SizeOf<E2Long>() != 8) return 9;
        if (Unsafe.SizeOf<EObj>() != 16) return 10;

        if (Unsafe.SizeOf<SeqObj>() != 16) return 11;
        if (Unsafe.SizeOf<AutoSized>() != 4) return 12;

        if (Unsafe.SizeOf<ContainsS13>() != 24) return 13;

        // Native layout takes a declared Size by the same rule, through the same CoreCLR helper
        // (`CollectNativeLayoutFieldMetadataThrowing`, classlayoutinfo.cpp:939-977), so the
        // marshalling size moves with the managed one.
        if (Marshal.SizeOf<S13>() != 13) return 14;
        if (Marshal.SizeOf<S0>() != 16) return 15;
        if (Marshal.SizeOf<SSmall>() != 12) return 16;
        if (Marshal.SizeOf<MByte3>() != 3) return 17;
        if (Marshal.SizeOf<ME13>() != 13) return 18;

        return 0;
    }
}
