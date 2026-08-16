using System;

// GCMemoryInfo's two span-valued properties, GenerationInfo and PauseDurations.
//
// CoreLib implements both with MemoryMarshal.CreateReadOnlySpan(ref _generationInfo0, 5) /
// (ref _pauseDuration0, 2) -- i.e. it takes a byref to the *first* of a run of sibling fields
// and walks forward by sizeof(element), relying on [StructLayout(Sequential)] having laid them
// out contiguously. PawPrint models a heap object as named field cells rather than a byte
// block, so every index past 0 is an access that leaves the cell its byref is rooted at: the
// byref access path has to recognise that a class field is a *view* into its object and serve
// the read from the object. That is issue #729; without it this guest stops at
// GenerationInfo[1] with "byte-view read at offset 32 for 32 bytes does not fit in single
// primitive cell of size 32". Length and index 0 work regardless.
//
// Differentially safe: every assertion below is a bound that holds no matter how many real
// GCs have happened by the time this runs. Sizes and fragmentations are byte counts, never
// negative; pause durations are elapsed times, never negative.
public class GCMemoryInfoSpanProperties
{
    public static int Main (string[] argv)
    {
        GCMemoryInfo info = GC.GetGCMemoryInfo ();

        if (info.GenerationInfo.Length != 5)
        {
            return 1;
        }

        for (int i = 0; i < info.GenerationInfo.Length; i++)
        {
            GCGenerationInfo gen = info.GenerationInfo[i];

            if (gen.SizeBeforeBytes < 0) return 100 + i;
            if (gen.FragmentationBeforeBytes < 0) return 200 + i;
            if (gen.SizeAfterBytes < 0) return 300 + i;
            if (gen.FragmentationAfterBytes < 0) return 400 + i;
        }

        if (info.PauseDurations.Length != 2)
        {
            return 2;
        }

        for (int i = 0; i < info.PauseDurations.Length; i++)
        {
            if (info.PauseDurations[i] < TimeSpan.Zero)
            {
                return 500 + i;
            }
        }

        return 0;
    }
}
