using System;

// `ReadOnlySpan<byte>.op_Equality` on UTF-8 literals, which are backed by a PE byte range
// (a field RVA) rather than by an array or a string. Slicing one leaves a byte cursor on
// that root, so this is the shape where "did the cursor leave the root's extent" must be
// answered from the range's own size rather than refused: a PE range knows how long it is.
//
// A byref-comparison rule that groups PE-backed roots with the roots that have no stride to
// fold a cursor into refuses `"abc"u8.Slice(1) == "xy"u8` — two spans of equal length, so
// `op_Equality` goes on to compare the references — instead of answering `false`. Every other
// span test is array- or string-rooted, and both of those roots fold, so only this file covers
// the PE-backed case.
public static class Utf8LiteralSpanEquality
{
    public static int Main(string[] args)
    {
        ReadOnlySpan<byte> abc = "abc"u8;
        ReadOnlySpan<byte> xy = "xy"u8;

        // A cursor on one range against a different range with no cursor. Both slices are
        // length 2, so this really does reach the reference comparison rather than
        // short-circuiting on length.
        if (abc.Slice(1) == xy)
        {
            return 1;
        }

        // The same shape with the operands the other way round, so a rule that only looks
        // for a cursor on the left-hand side fails here.
        if (xy == abc.Slice(1))
        {
            return 2;
        }

        // Undisplaced literal against itself: Roslyn emits one field RVA per distinct
        // literal, so these are the same root and compare equal. (If that ever stopped
        // being true, this returns 3 on the real runtime too, and the harness's expected
        // return code of 0 fails rather than the two runtimes quietly agreeing on 3.)
        ReadOnlySpan<byte> abcAgain = "abc"u8;
        if (!(abc == abcAgain))
        {
            return 3;
        }

        // Equal cursors on one root.
        if (!(abc.Slice(1) == abcAgain.Slice(1)))
        {
            return 4;
        }

        // Different cursors on one root, equal lengths: same-root arithmetic, 1 != 2.
        if (abc.Slice(1, 1) == abc.Slice(2, 1))
        {
            return 5;
        }

        // Zero-length slices, each with a cursor strictly inside its own range. Length no
        // longer separates them, so the answer rests entirely on the two references.
        if (abc.Slice(1, 0) == xy.Slice(1, 0))
        {
            return 6;
        }

        return 0;
    }
}
