using System;
using System.Runtime.CompilerServices;

// Writes into a string's characters whose outcome CoreCLR gives the same way on every JIT tier.
// Writing a different character into an interned string is the row that is not, and PawPrint
// refuses it (`TestInternedStringWrites.fs` says why).
class Program
{
    // Not inlined, so that real .NET materialises the literal only when this is first called.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static string Literal() => "qqqqqqqqqqqqqqqqqqqqqqq";

    static unsafe int Main(string[] args)
    {
        // A string nothing has interned may be written, and the write is visible.
        string fresh = new string('q', 23);
        fixed (char* p = fresh) p[0] = 'r';
        if (fresh[0] != 'r') return 1;

        // Writing it does not put it, or its old value, in the intern table.
        if (string.IsInterned(fresh) != null) return 2;

        // Writing an interned string's characters with the values they already hold changes nothing.
        string literal = Literal();
        fixed (char* p = literal)
        {
            p[0] = 'q';
            // The null terminator is not part of the value a lookup compares.
            p[literal.Length] = 'x';
            p[literal.Length] = '\0';
        }

        if (!ReferenceEquals(string.IsInterned(new string('q', 23)), literal)) return 3;
        if (!ReferenceEquals(Literal(), literal)) return 4;

        // A fresh string written to an interned value is still not the interned object.
        string written = new string('a', 23);
        fixed (char* p = written)
        {
            for (int i = 0; i < written.Length; i++) p[i] = 'q';
        }

        if (!ReferenceEquals(string.Intern(written), literal)) return 5;

        return 0;
    }
}
