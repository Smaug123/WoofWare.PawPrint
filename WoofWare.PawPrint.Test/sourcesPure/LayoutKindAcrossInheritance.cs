using System;
using System.Runtime.InteropServices;

// A type's layout kind governs *its own* fields. PawPrint flattens a reference type's whole base
// chain into one field list before laying it out (issue #994), so that list routinely mixes
// fields governed by different kinds: an inherited field of a sequential base carries no
// `FieldOffset`, and one of an explicit-layout base carries one, whatever the derived type
// declares. Both hierarchies below load on real .NET.
//
// This is the shape that makes "the declared kind and the field shape must agree" false as a
// whole-list property, which is why the layout router reads the field shape structurally and
// consults the declared kind only to choose between auto and sequential placement.
public class TestLayoutKindAcrossInheritance
{
    [StructLayout(LayoutKind.Sequential)] private class SeqBase { public int X; }

    // Explicit layout with no instance fields of its own, so every field in the flattened list is
    // an inherited one carrying no `FieldOffset`.
    [StructLayout(LayoutKind.Explicit)] private sealed class ExplicitDerived : SeqBase { }

    [StructLayout(LayoutKind.Explicit)] private class ExplicitBase { [FieldOffset(0)] public int A; }

    // The other direction: the flattened list is entirely offset-carrying while the derived type
    // declares sequential layout.
    [StructLayout(LayoutKind.Sequential)] private sealed class SeqDerived : ExplicitBase { }

    public static int Main(string[] argv)
    {
        ExplicitDerived d = new ExplicitDerived();
        d.X = 7;
        if (d.X != 7) return 1;

        SeqDerived s = new SeqDerived();
        s.A = 9;
        if (s.A != 9) return 2;

        return 0;
    }
}
