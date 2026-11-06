using System;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace StringIntern
{
    class Program
    {
        // Built at run time so that no `ldstr` anywhere names the value: real .NET interns every
        // literal of a method when it JITs the method, which would put the value in the intern
        // table before the guest asks about it.
        static string Fresh(char c, int count) => new string(c, count);

        // Not inlined, so that real .NET JITs this body, and so materialises its literal, only when
        // it is first called; inlined into `Main`, the literal would be interned before `Main` runs.
        [MethodImpl(MethodImplOptions.NoInlining)]
        static string ZedLiteral() => "zzz";

        static int Main(string[] args)
        {
            // Including for a zero-length string that is not `string.Empty`, which only the runtime's
            // own allocator makes: every public route to an empty string answers `string.Empty`.
            MethodInfo fastAllocate = typeof(string).GetMethod(
                "FastAllocateString",
                BindingFlags.NonPublic | BindingFlags.Static,
                new[] { typeof(nint) })!;
            string strayEmpty = (string)fastAllocate.Invoke(null, new object[] { (nint)0 })!;
            // Both asked before this method names `string.Empty` or "" itself.
            string? strayIsInterned = string.IsInterned(strayEmpty);
            string strayInterned = string.Intern(strayEmpty);
            if (strayEmpty.Length != 0) return 16;
            if (ReferenceEquals(strayEmpty, string.Empty)) return 17;
            if (!ReferenceEquals(strayIsInterned, string.Empty)) return 18;
            if (!ReferenceEquals(strayInterned, string.Empty)) return 19;
            if (!ReferenceEquals(string.Empty, "")) return 20;

            // A value nothing has interned: `IsInterned` says so, and `Intern` interns the very
            // object it is given.
            string first = Fresh('q', 7);
            if (string.IsInterned(first) != null) return 1;
            if (!ReferenceEquals(string.Intern(first), first)) return 2;

            // An equal but distinct object then finds the first.
            string second = Fresh('q', 7);
            if (ReferenceEquals(second, first)) return 3;
            if (!ReferenceEquals(string.IsInterned(second), first)) return 4;
            if (!ReferenceEquals(string.Intern(second), first)) return 5;
            if (!ReferenceEquals(string.IsInterned(first), first)) return 6;

            // A copy of a literal finds the literal.
            string literal = "intern me";
            string copy = new string(literal.AsSpan());
            if (ReferenceEquals(copy, literal)) return 7;
            if (!ReferenceEquals(string.IsInterned(copy), literal)) return 8;
            if (!ReferenceEquals(string.Intern(copy), literal)) return 9;

            // A literal materialised after its value was interned is the interned object.
            string zed = Fresh('z', 3);
            if (!ReferenceEquals(string.Intern(zed), zed)) return 10;
            if (!ReferenceEquals(ZedLiteral(), zed)) return 11;

            // The empty string is interned from the start, as `string.Empty`.
            if (!ReferenceEquals(string.IsInterned(Fresh('e', 0)), string.Empty)) return 12;
            if (!ReferenceEquals(string.Intern(Fresh('e', 0)), string.Empty)) return 13;


            // Both reject null before reaching the runtime.
            try
            {
                string.Intern(null!);
                return 14;
            }
            catch (ArgumentNullException e) when (e.ParamName == "str")
            {
            }

            try
            {
                string.IsInterned(null!);
                return 15;
            }
            catch (ArgumentNullException e) when (e.ParamName == "str")
            {
            }

            return 0;
        }
    }
}
