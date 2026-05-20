using System;

class Program
{
    static int Main(string[] args)
    {
        // Exercises the path where CoreLib calls FastAllocateString and then
        // initialises the freshly-allocated string via `result._firstChar = c`
        // (a direct stfld on the String._firstChar field). The interpreter must
        // keep the StringContents / StringArrayData side tables in sync with
        // that field write, otherwise indexer reads return the unsynchronised
        // NUL even though `_firstChar` itself was updated.
        string s = 'x'.ToString();

        if (s.Length != 1) return 1;
        if (s[0] != 'x') return 2;

        return 0;
    }
}
