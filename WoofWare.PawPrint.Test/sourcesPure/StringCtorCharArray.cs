using System;

// CoreCLR implements `new string(char[])` and `new string(char[], int, int)` by
// redirecting the constructor's ECall to the managed static `String.Ctor(...)`
// (see `ecall.cpp`, `PopulateManagedStringConstructors`), so every behaviour
// asserted here is produced by CoreLib's own IL rather than by the runtime.
class StringCtorCharArray
{
    static int TestWholeArray()
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o' };
        string s = new string(chars);
        if (s.Length != 5)
            return 1;
        if (s != "hello")
            return 2;

        return 0;
    }

    static int TestMutationDoesNotAlias()
    {
        // The constructor copies; later writes to the array must not be visible.
        char[] chars = { 'a', 'b', 'c' };
        string s = new string(chars);
        chars[0] = 'z';
        if (s != "abc")
            return 10;

        return 0;
    }

    static int TestNullArrayIsCanonicalEmpty()
    {
        // Ctor(char[]) returns String.Empty for a null or zero-length array.
        string s = new string((char[])null);
        if (s.Length != 0)
            return 20;
        if (!ReferenceEquals(s, ""))
            return 21;

        return 0;
    }

    static int TestEmptyArrayIsCanonicalEmpty()
    {
        string s = new string(new char[0]);
        if (s.Length != 0)
            return 30;
        if (!ReferenceEquals(s, ""))
            return 31;

        return 0;
    }

    static int TestEmbeddedNullIsPreserved()
    {
        // Unlike the char* overload, the array overload has an explicit length
        // and so keeps interior NULs.
        char[] chars = { 'a', '\0', 'b' };
        string s = new string(chars);
        if (s.Length != 3)
            return 40;
        if (s[1] != '\0')
            return 41;

        return 0;
    }

    static int TestRange()
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o' };
        if (new string(chars, 1, 3) != "ell")
            return 50;
        if (new string(chars, 0, 5) != "hello")
            return 51;
        if (new string(chars, 5, 0) != "")
            return 52;
        if (!ReferenceEquals(new string(chars, 2, 0), ""))
            return 53;

        return 0;
    }

    // The out-of-range and null-argument cases live in StringCtorArgumentValidation.cs,
    // which is parked on an unrelated `[InlineArray]` gap in the throw helpers' message
    // formatting.

    static int Main(string[] args)
    {
        int result = TestWholeArray();
        if (result != 0)
            return result;

        result = TestMutationDoesNotAlias();
        if (result != 0)
            return result;

        result = TestNullArrayIsCanonicalEmpty();
        if (result != 0)
            return result;

        result = TestEmptyArrayIsCanonicalEmpty();
        if (result != 0)
            return result;

        result = TestEmbeddedNullIsPreserved();
        if (result != 0)
            return result;

        result = TestRange();
        if (result != 0)
            return result;

        return 0;
    }
}
