using System;
using System.Collections.Generic;

namespace RuntimeTypeMetadataTokenGenericParameter
{
    class Box<T> { }

    class Pair<TKey, TValue> { }

    class Program
    {
        static int Main(string[] args)
        {
            int boxT = typeof(Box<>).GetGenericArguments()[0].MetadataToken;
            int pairK = typeof(Pair<,>).GetGenericArguments()[0].MetadataToken;
            int pairV = typeof(Pair<,>).GetGenericArguments()[1].MetadataToken;

            // ECMA-335 §II.22.20: GenericParam table tag is 0x2A.
            if ((boxT >> 24) != 0x2A) return 1;
            if ((pairK >> 24) != 0x2A) return 2;
            if ((pairV >> 24) != 0x2A) return 3;

            // Distinct rows for distinct parameters.
            if (boxT == pairK) return 4;
            if (pairK == pairV) return 5;
            if (boxT == pairV) return 6;

            // Same parameter returns the same token across two reflection lookups.
            if (typeof(Box<>).GetGenericArguments()[0].MetadataToken != boxT) return 7;

            // CoreLib parameters route through the same path; smoke-check the tag.
            if ((typeof(List<>).GetGenericArguments()[0].MetadataToken >> 24) != 0x2A) return 8;

            return 0;
        }
    }
}
