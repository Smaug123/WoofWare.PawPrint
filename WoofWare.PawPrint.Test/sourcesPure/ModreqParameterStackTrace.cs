using System;

// Roslyn emits `modreq(System.Runtime.InteropServices.InAttribute)` on an `in` parameter only where
// the signature participates in override/implement matching: abstract/virtual/override methods,
// interface members, and delegate Invoke. A plain non-virtual method gets a bare `int32&` and so
// does NOT exercise the custom-modifier path at all.
abstract class ModreqBase
{
    public abstract void Boom(in int x);
}

class ModreqDerived : ModreqBase
{
    public override void Boom(in int x)
    {
        throw new InvalidOperationException("boom");
    }
}

class ModreqParameterStackTrace
{
    static bool ContainsSubstring(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                return true;
            }
        }

        return false;
    }

    static int Main(string[] args)
    {
        ModreqBase b = new ModreqDerived();

        try
        {
            b.Boom(1);
        }
        catch (InvalidOperationException ex)
        {
            string trace = ex.StackTrace;
            if (trace == null)
            {
                return 10;
            }

            // The custom modifier is an annotation on the signature, not the parameter's type: the
            // frame must render the type the modifier is attached to (`Int32&`), never the modifier
            // itself (`InAttribute`).
            if (!ContainsSubstring(trace, "ModreqDerived.Boom(Int32& x)"))
            {
                return 11;
            }

            if (ContainsSubstring(trace, "InAttribute"))
            {
                return 12;
            }

            return 0;
        }

        return 1;
    }
}
