using System;

class TypeMetadataTokenTests
{
    class Nested
    {
    }

    class Box<T>
    {
    }

    static int Main(string[] args)
    {
        int result = 0;

        int selfToken = typeof(TypeMetadataTokenTests).MetadataToken;
        int nestedToken = typeof(Nested).MetadataToken;
        int openGenericToken = typeof(Box<>).MetadataToken;
        int closedGenericToken = typeof(Box<int>).MetadataToken;
        int arrayToken = typeof(int[]).MetadataToken;

        if ((selfToken & unchecked((int)0xFF000000)) != 0x02000000)
        {
            result |= 1;
        }

        if ((nestedToken & unchecked((int)0xFF000000)) != 0x02000000)
        {
            result |= 2;
        }

        if (selfToken == nestedToken)
        {
            result |= 4;
        }

        if (openGenericToken != closedGenericToken)
        {
            result |= 8;
        }

        if (arrayToken != 0x02000000)
        {
            result |= 16;
        }

        return result;
    }
}
