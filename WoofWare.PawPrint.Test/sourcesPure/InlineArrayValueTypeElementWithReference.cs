using System;
using System.Runtime.CompilerServices;

// An `[InlineArray(N)]` whose element is a *struct that contains a reference*, rather than a bare
// reference or a byte-addressable primitive. This is the one element shape the N-slot layout does
// not make reachable, and it is a limit of the byref reinterpret elision rather than of the layout:
//
//   - the layout is right (the sweep in `TestInlineArrayLayout.fs` covers a `{byte; object}`
//     element and agrees with the real runtime on every size);
//   - `buffer[k]` is `[ReinterpretAs Elem; ByteOffset k * sizeof(Elem)]`, and
//     `TryFieldExactlyCovering` does find slot `k` exactly;
//   - but `isLayoutCompatibleForElision` (IlMachineManagedByref.fs) is object-reference identity
//     only, so it declines, and the bytewise fallback then fails in `reinterpretStorageBytes`
//     because a value type containing object references has no byte rendering.
//
// This is not specific to inline arrays and predates them: on `main` — where the struct's storage
// was its one declared field — the *first* element diverges identically, with the same
// "write through `ReinterpretAs` over byte-unaddressable storage (value type containing object
// references)" failure. Un-parking it needs the elision predicate widened to accept two cells of
// the same declared value type, which changes the `Unsafe.As`-wrapper classifier for every caller
// and so wants its own change.
public class TestInlineArrayValueTypeElementWithReference
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer { private Elem _item; }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;

        buffer[0] = new Elem { Tag = 1, Payload = new Box { V = 10 } };
        buffer[1] = new Elem { Tag = 2, Payload = new Box { V = 20 } };

        if (buffer[0].Tag != 1 || buffer[0].Payload.V != 10) return 1;
        if (buffer[1].Tag != 2 || buffer[1].Payload.V != 20) return 2;

        return 0;
    }
}
