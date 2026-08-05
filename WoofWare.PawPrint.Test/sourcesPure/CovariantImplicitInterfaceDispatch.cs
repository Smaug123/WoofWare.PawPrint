// The mirror image of `ContravariantImplicitInterfaceDispatch.cs`: covariant (`out`) interface
// dispatch onto an IMPLICIT interface implementation.
//
// This case passed before the variant-interface-map retarget landed, but only by accident of
// where the two halves of the signature comparison differ: `signatureMatchesTarget` compares
// return types with `isAssignableFrom` and parameter types with equality, so the covariant half
// of variance was tolerated and the contravariant half was not. It is here so that a future
// tightening of the return-type comparison cannot silently break covariant dispatch without a
// test noticing.

using System;

interface ISource<out T>
{
    T Get();
}

// Implemented at `string`, so dispatching through `ISource<object>` (legal under `out`
// variance) must reach a body whose return type is `string`, not `object`.
sealed class StringSource : ISource<string>
{
    public int Calls;

    public string Get()
    {
        Calls++;
        return "covariant";
    }
}

class Program
{
    static int Main(string[] args)
    {
        StringSource source = new StringSource();
        ISource<object> asObjectSource = source;

        object result = asObjectSource.Get();

        if (!(result is string s) || s != "covariant") return 1;
        if (source.Calls != 1) return 2;

        // Dispatching through the declared instantiation must reach the same body.
        ISource<string> asStringSource = source;
        if (asStringSource.Get() != "covariant") return 3;
        if (source.Calls != 2) return 4;

        return 0;
    }
}
