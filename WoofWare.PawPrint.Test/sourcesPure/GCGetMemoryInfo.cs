using System;

// Every assertion here must also hold on a real CLR: PawPrint's test harness diffs the
// exit code against the real runtime, and real heap sizes / memory loads are not
// deterministic. So this only checks structural invariants that are true regardless of
// whether, or how many, real GCs have happened by the time this runs -- never a specific
// numeric value.
//
// Every assertion is on a SINGLE field of a single snapshot. Nothing here relates two
// fields to each other, and that restriction is load-bearing rather than stylistic.
// GC.GetMemoryInfo is an InternalCall that fills the caller's GCMemoryInfoData field by
// field (comutilnative.cpp, GCInterface::GetMemoryInfo -> gc.cpp,
// GCHeap::GetMemoryInfo) out of a `last_recorded_gc_info` record, with no synchronisation.
// A *blocking* GC cannot interleave with that, since it suspends the EE -- but a
// background GC runs concurrently with managed code, and both GCKind.Background and
// GCKind.Any (when the last recorded collection was a BGC) read the BGC record through
// `get_completed_bgc_info()`. A BGC completing mid-call is therefore observable as a torn
// record: some fields from the old all-zero record, some from the new one.
//
// So any cross-field assertion is a race, however obviously true it looks of a coherent
// snapshot. Three were tried here and removed after this test was seen to fail
// intermittently in full-suite runs:
//   - "Index == 0 implies every other field is 0" (the documented never-collected state);
//   - FragmentedBytes <= HeapSizeBytes;
//   - GenerationInfo[0].FragmentationAfterBytes <= SizeAfterBytes.
// Each holds of any single coherent record -- gc.cpp:51405-51406 writes heap_size and
// fragmentation together at the end of a collection -- and each can still be violated by
// reading two fields either side of a BGC completion.
//
// PawPrint's own contract (every field zero, always, because it never collects) is
// therefore NOT testable here: it is not a property of the real runtime, so it cannot be
// asserted in a differential test. sourcesImpure/GCMemoryInfoAllZero.cs pins it instead,
// where the expected exit code is PawPrint's alone.
public class GCGetMemoryInfo
{
    // Takes an already-fetched snapshot rather than a GCKind, so that no assertion anywhere
    // in this file compares two snapshots taken at different times either.
    static int TestStructuralInvariants (GCMemoryInfo info, int offset)
    {
        // None of these are ever negative on any real platform: each is a byte count, an
        // object count, or a monotone index.
        if (info.Index < 0) return offset + 12;
        if (info.HeapSizeBytes < 0) return offset + 13;
        if (info.FragmentedBytes < 0) return offset + 14;
        if (info.TotalCommittedBytes < 0) return offset + 15;
        if (info.PromotedBytes < 0) return offset + 16;
        if (info.PinnedObjectsCount < 0) return offset + 17;
        if (info.FinalizationPendingCount < 0) return offset + 18;
        if (info.Generation < 0) return offset + 19;
        if (info.MemoryLoadBytes < 0) return offset + 30;

        // A percentage, so bounded on both sides on any real platform.
        if (info.PauseTimePercentage < 0) return offset + 20;
        if (info.PauseTimePercentage > 100) return offset + 21;

        // Reach into the *struct-typed* fields too, not just the scalars. These are written
        // by a different code path in the handler (a type-driven recursive zero via the
        // field's own declared signature, rather than a literal), so without touching them a
        // regression in that path -- a wrong field name, or a zero of the wrong shape --
        // would go uncaught.
        //
        // Index 0 only: sibling-field spans don't work past the first element (issue #729),
        // which GCMemoryInfoSpanProperties.cs pins separately in the `unimplemented` set.
        // Length is safe to read, and a fixed 5/2 on every platform.
        if (info.GenerationInfo.Length != 5) return offset + 22;
        if (info.PauseDurations.Length != 2) return offset + 23;

        GCGenerationInfo gen0 = info.GenerationInfo[0];

        if (gen0.SizeBeforeBytes < 0) return offset + 24;
        if (gen0.FragmentationBeforeBytes < 0) return offset + 25;
        if (gen0.SizeAfterBytes < 0) return offset + 26;
        if (gen0.FragmentationAfterBytes < 0) return offset + 27;

        if (info.PauseDurations[0] < TimeSpan.Zero) return offset + 29;

        return 0;
    }

    public static int Main (string[] argv)
    {
        int result;

        result = TestStructuralInvariants (GC.GetGCMemoryInfo (GCKind.Any), 100);
        if (result != 0)
        {
            return result;
        }

        result = TestStructuralInvariants (GC.GetGCMemoryInfo (GCKind.Ephemeral), 200);
        if (result != 0)
        {
            return result;
        }

        result = TestStructuralInvariants (GC.GetGCMemoryInfo (GCKind.FullBlocking), 300);
        if (result != 0)
        {
            return result;
        }

        result = TestStructuralInvariants (GC.GetGCMemoryInfo (GCKind.Background), 400);
        if (result != 0)
        {
            return result;
        }

        // The parameterless overload forwards to GCKind.Any. It gets the same structural
        // checks, rather than being compared against a separately-taken GCKind.Any snapshot:
        // such a comparison would race the real runtime's first collection.
        result = TestStructuralInvariants (GC.GetGCMemoryInfo (), 500);
        if (result != 0)
        {
            return result;
        }

        return 0;
    }
}
