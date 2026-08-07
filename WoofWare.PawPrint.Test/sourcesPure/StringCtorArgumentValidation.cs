using System;

// The argument-validation half of the string constructors, split out from the
// happy-path files (StringCtorCharArray.cs, StringCtorCharCount.cs,
// StringCtorCharPointerRange.cs, StringCtorSbytePointer.cs) because every case
// here is blocked on the same *unrelated* gap: the `ArgumentOutOfRangeException`
// throw helpers format their message with `SR.Format(fmt, value, other)`, which
// builds a `System.TwoObjects` — an `[InlineArray(2)]` struct of `object?` —
// and writes through it, which PawPrint cannot yet model.
unsafe class StringCtorArgumentValidation
{
    static int TestCharArrayRangeNullThrows()
    {
        try
        {
            string s = new string((char[])null, 0, 0);
            return 1;
        }
        catch (ArgumentNullException)
        {
            return 0;
        }
    }

    static int TestCharArrayRangeOutOfRangeThrows()
    {
        char[] chars = { 'a', 'b', 'c' };

        try
        {
            string s = new string(chars, -1, 1);
            return 10;
        }
        catch (ArgumentOutOfRangeException)
        {
        }

        try
        {
            string s = new string(chars, 0, -1);
            return 11;
        }
        catch (ArgumentOutOfRangeException)
        {
        }

        try
        {
            string s = new string(chars, 2, 2);
            return 12;
        }
        catch (ArgumentOutOfRangeException)
        {
        }

        return 0;
    }

    static int TestNegativeCountThrows()
    {
        try
        {
            string s = new string('x', -1);
            return 20;
        }
        catch (ArgumentOutOfRangeException)
        {
            return 0;
        }
    }

    static int TestCharPointerNullWithLengthThrows()
    {
        try
        {
            string s = new string((char*)null, 0, 1);
            return 30;
        }
        catch (ArgumentOutOfRangeException)
        {
            return 0;
        }
    }

    static int TestCharPointerNegativeArgumentsThrow()
    {
        char[] chars = { 'a', 'b', 'c' };
        fixed (char* p = chars)
        {
            try
            {
                string s = new string(p, 0, -1);
                return 40;
            }
            catch (ArgumentOutOfRangeException)
            {
            }

            try
            {
                string s = new string(p, -1, 1);
                return 41;
            }
            catch (ArgumentOutOfRangeException)
            {
            }
        }

        return 0;
    }

    static int TestSbytePointerNullWithLengthThrows()
    {
        try
        {
            string s = new string((sbyte*)null, 0, 1);
            return 50;
        }
        catch (ArgumentNullException)
        {
            return 0;
        }
    }

    static int TestSbytePointerNegativeArgumentsThrow()
    {
        byte[] bytes = { (byte)'a', (byte)'b', (byte)'c' };
        fixed (byte* b = bytes)
        {
            try
            {
                string s = new string((sbyte*)b, -1, 1);
                return 60;
            }
            catch (ArgumentOutOfRangeException)
            {
            }

            try
            {
                string s = new string((sbyte*)b, 0, -1);
                return 61;
            }
            catch (ArgumentOutOfRangeException)
            {
            }
        }

        return 0;
    }

    static int Main(string[] args)
    {
        int result = TestCharArrayRangeNullThrows();
        if (result != 0)
            return result;

        result = TestCharArrayRangeOutOfRangeThrows();
        if (result != 0)
            return result;

        result = TestNegativeCountThrows();
        if (result != 0)
            return result;

        result = TestCharPointerNullWithLengthThrows();
        if (result != 0)
            return result;

        result = TestCharPointerNegativeArgumentsThrow();
        if (result != 0)
            return result;

        result = TestSbytePointerNullWithLengthThrows();
        if (result != 0)
            return result;

        result = TestSbytePointerNegativeArgumentsThrow();
        if (result != 0)
            return result;

        return 0;
    }
}
