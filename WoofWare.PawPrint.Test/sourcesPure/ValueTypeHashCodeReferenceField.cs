using System;

// `ValueType.GetHashCode` on a struct that is not bit-comparable asks the runtime
// (`ValueType_GetHashCodeStrategy`) which single field to hash. The hash *value* mixes in the
// MethodTable pointer and a per-process seed, so it is not comparable across runtimes; which
// field was selected is, and shows up as "varying this field moves the hash, varying that one
// does not".
public class Program
{
    // A reference type whose hash we control, so the assertions below do not depend on how
    // strings hash.
    private sealed class Ref
    {
        private readonly int hash;

        public Ref(int hash)
        {
            this.hash = hash;
        }

        public override int GetHashCode() => hash;

        public override bool Equals(object obj) => obj is Ref other && other.hash == hash;
    }

    private struct RefThenInt
    {
        public Ref R;
        public int A;
    }

    private struct TwoRefs
    {
        public Ref First;
        public Ref Second;
    }

    public static int Main(string[] args)
    {
        // ReferenceField: the first declared field is a non-null reference, so the hash comes
        // from that reference and from nothing else in the struct.
        RefThenInt sameRef1 = new RefThenInt { R = new Ref(7), A = 1 };
        RefThenInt sameRef2 = new RefThenInt { R = new Ref(7), A = 2 };
        if (sameRef1.GetHashCode() != sameRef2.GetHashCode())
        {
            return 1;
        }

        RefThenInt otherRef = new RefThenInt { R = new Ref(8), A = 1 };
        if (sameRef1.GetHashCode() == otherRef.GetHashCode())
        {
            return 2;
        }

        // A null reference field is skipped, so the walk lands on the `int` behind it and hashes
        // that instead.
        RefThenInt nullRef1 = new RefThenInt { R = null, A = 1 };
        RefThenInt nullRef2 = new RefThenInt { R = null, A = 2 };
        if (nullRef1.GetHashCode() == nullRef2.GetHashCode())
        {
            return 3;
        }

        RefThenInt nullRefAgain = new RefThenInt { R = null, A = 1 };
        if (nullRef1.GetHashCode() != nullRefAgain.GetHashCode())
        {
            return 4;
        }

        // A null reference field is skipped and the walk moves on to the next declared field.
        TwoRefs skipToSecond1 = new TwoRefs { First = null, Second = new Ref(11) };
        TwoRefs skipToSecond2 = new TwoRefs { First = null, Second = new Ref(11) };
        if (skipToSecond1.GetHashCode() != skipToSecond2.GetHashCode())
        {
            return 5;
        }

        TwoRefs skipToDifferentSecond = new TwoRefs { First = null, Second = new Ref(12) };
        if (skipToSecond1.GetHashCode() == skipToDifferentSecond.GetHashCode())
        {
            return 6;
        }

        // A non-null first field stops the walk, so the second field cannot contribute.
        TwoRefs firstWins1 = new TwoRefs { First = new Ref(11), Second = new Ref(12) };
        TwoRefs firstWins2 = new TwoRefs { First = new Ref(11), Second = new Ref(13) };
        if (firstWins1.GetHashCode() != firstWins2.GetHashCode())
        {
            return 7;
        }

        // Every field a null reference: no field contributes at all, and in particular the null
        // is neither dereferenced nor hashed as bytes.
        TwoRefs allNull1 = new TwoRefs { First = null, Second = null };
        TwoRefs allNull2 = new TwoRefs { First = null, Second = null };
        if (allNull1.GetHashCode() != allNull2.GetHashCode())
        {
            return 8;
        }

        // ... which is a different hash from the one a contributing second field produces.
        if (allNull1.GetHashCode() == skipToSecond1.GetHashCode())
        {
            return 9;
        }

        return 0;
    }
}
