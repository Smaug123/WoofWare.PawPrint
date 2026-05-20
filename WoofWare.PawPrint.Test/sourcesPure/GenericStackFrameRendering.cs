using System;

class Container<TC>
{
    public static void Throw<TM>(TC c, TM m)
    {
        throw new InvalidOperationException("boom");
    }
}

class GenericStackFrameRendering
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
        try
        {
            Container<int>.Throw<string>(42, "hi");
        }
        catch (InvalidOperationException ex)
        {
            string trace = ex.StackTrace;
            if (trace == null)
            {
                return 10;
            }

            // Generic type's runtime Type.Name keeps the arity suffix `1; the
            // type-argument list is not rendered. The method-argument list IS rendered as
            // [TM] using the formal name. Parameter types use the formal generic-parameter
            // names (TC and TM), not the substituted call-site arguments — this matches
            // CoreCLR's shared-generics stack-frame rendering.
            if (!ContainsSubstring(trace, "Container`1.Throw[TM](TC c, TM m)"))
            {
                return 11;
            }

            if (!ContainsSubstring(trace, "GenericStackFrameRendering.Main(String[] args)"))
            {
                return 12;
            }

            return 0;
        }

        return 1;
    }
}
