using System;

// Pins PawPrint's actual GC.GetMemoryInfo contract: every field zero, for every GCKind,
// always -- because the interpreter never performs a collection, so "what did the last GC
// of this kind record" has no answer other than "there has never been one".
//
// This is deliberately an *impure* case. It cannot live in sourcesPure, where the harness
// diffs the exit code against the real runtime: a real CLR reports genuine heap sizes and
// a genuine memory load, so it would disagree with every assertion below. That is not a
// PawPrint bug, it is the whole reason the contract is PawPrint's alone and has to be
// asserted somewhere the real runtime is not consulted.
//
// The companion sourcesPure/GCGetMemoryInfo.cs covers the cross-runtime half (single-field
// structural invariants only -- see its header for why nothing there may relate two fields
// of one snapshot).
//
// Returns 42 rather than 0 on success, so that "the guest never really ran" cannot be
// mistaken for a pass.
public class GCMemoryInfoAllZero
{
    static int AssertAllZero (GCMemoryInfo info, int offset)
    {
        if (info.Index != 0) return offset + 1;
        if (info.HeapSizeBytes != 0) return offset + 2;
        if (info.FragmentedBytes != 0) return offset + 3;
        if (info.TotalCommittedBytes != 0) return offset + 4;
        if (info.PromotedBytes != 0) return offset + 5;
        if (info.PinnedObjectsCount != 0) return offset + 6;
        if (info.FinalizationPendingCount != 0) return offset + 7;
        if (info.MemoryLoadBytes != 0) return offset + 8;
        if (info.Generation != 0) return offset + 9;
        if (info.PauseTimePercentage != 0) return offset + 10;
        if (info.Compacted) return offset + 11;
        if (info.Concurrent) return offset + 12;

        // These two are the documented divergence from upstream: a real CLR computes them
        // unconditionally from physical memory rather than from any last-GC record, so no
        // real machine ever reports this pair as zero. PawPrint models no memory budget at
        // all. Asserted explicitly so that if a simulated budget is ever introduced, this
        // fails and forces the decision to be made deliberately rather than silently.
        if (info.HighMemoryLoadThresholdBytes != 0) return offset + 13;
        if (info.TotalAvailableMemoryBytes != 0) return offset + 14;

        // Struct-typed fields go through a different path in the handler (a type-driven
        // recursive zero via each field's own declared signature) than the scalars above.
        // Index 0 only: sibling-field spans do not work past the first element, issue #729.
        if (info.GenerationInfo.Length != 5) return offset + 15;
        if (info.PauseDurations.Length != 2) return offset + 16;

        GCGenerationInfo gen0 = info.GenerationInfo[0];

        if (gen0.SizeBeforeBytes != 0) return offset + 17;
        if (gen0.FragmentationBeforeBytes != 0) return offset + 18;
        if (gen0.SizeAfterBytes != 0) return offset + 19;
        if (gen0.FragmentationAfterBytes != 0) return offset + 20;

        if (info.PauseDurations[0] != TimeSpan.Zero) return offset + 21;

        return 0;
    }

    public static int Main (string[] argv)
    {
        int result;

        result = AssertAllZero (GC.GetGCMemoryInfo (GCKind.Any), 100);
        if (result != 0)
        {
            return result;
        }

        result = AssertAllZero (GC.GetGCMemoryInfo (GCKind.Ephemeral), 200);
        if (result != 0)
        {
            return result;
        }

        result = AssertAllZero (GC.GetGCMemoryInfo (GCKind.FullBlocking), 300);
        if (result != 0)
        {
            return result;
        }

        result = AssertAllZero (GC.GetGCMemoryInfo (GCKind.Background), 400);
        if (result != 0)
        {
            return result;
        }

        result = AssertAllZero (GC.GetGCMemoryInfo (), 500);
        if (result != 0)
        {
            return result;
        }

        return 42;
    }
}
