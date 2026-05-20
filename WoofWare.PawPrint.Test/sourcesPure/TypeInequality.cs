using System;

class TypeInequalityTests
{
    class Box<T>
    {
    }

    static int Main(string[] args)
    {
        int result = 0;

        Type intType = typeof(int);
        Type sameIntType = typeof(int);
        Type stringType = typeof(string);
        Type nullType = null;

        if (intType != sameIntType)
        {
            result |= 1;
        }

        if (!(intType != stringType))
        {
            result |= 2;
        }

        if (nullType != null)
        {
            result |= 4;
        }

        if (!(nullType != intType))
        {
            result |= 8;
        }

        if (typeof(Box<int>) != typeof(Box<int>))
        {
            result |= 16;
        }

        if (!(typeof(Box<int>) != typeof(Box<string>)))
        {
            result |= 32;
        }

        return result;
    }
}
