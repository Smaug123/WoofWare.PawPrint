using System;
using System.Collections;

// `Environment.GetEnvironmentVariables()`, which on CoreCLR runs
// Environment.Variables.Windows.cs on every platform: it takes an environment
// block from the `GetEnvironmentStringsW` QCall, walks it into a Hashtable, and
// releases it through `FreeEnvironmentStringsW` in a finally.
//
// Pure, so every assertion below has to be true of *both* runtimes, and the two
// environments differ: under PawPrint it is the seeded kernel table, and under
// the real runtime it is whatever the test host was started with. So nothing
// here names a variable or a count. What it does pin is the relationship
// between the two environment APIs, which is a fact about the block format and
// the parse of it rather than about any particular table — and it is exactly the
// relationship a wrongly-built block breaks, since an entry split at the wrong
// '=' yields a name the table does not hold.
//
// The PawPrint-only half — that the dictionary is exactly the configured table —
// is asserted by `GetEnvironmentVariables reports exactly the emulated
// environment` in TestPureCases, which can name values because it configured
// them.
public class TestEnvironmentGetVariables
{
    public static int Main(string[] argv)
    {
        IDictionary vars = Environment.GetEnvironmentVariables();

        if (vars == null) return 1;

        // Deliberately no "the dictionary is non-empty" assertion. An empty
        // process environment is legal — `env -i` produces one — so requiring
        // otherwise would be asserting a property of however the suite was
        // invoked rather than a property of either runtime, and the two sides
        // would disagree: PawPrint always has its seeded
        // DOTNET_SYSTEM_GLOBALIZATION_INVARIANT, while the oracle inherits the
        // test host's environment and could in principle have none.
        //
        // That leaves the loop below able to run zero times, so it is not what
        // stops this file passing vacuously. What does is the PawPrint-only
        // `GetEnvironmentVariables reports exactly the emulated environment` in
        // TestPureCases: it asserts an exact count, which is strictly stronger,
        // and it is what kills an implementation whose block yields nothing.

        int seen = 0;

        foreach (DictionaryEntry entry in vars)
        {
            string key = entry.Key as string;
            string value = entry.Value as string;

            if (key == null) return 2;
            if (value == null) return 3;

            // CoreLib discards any entry whose first '=' is not after the first
            // character, so no well-formed block can yield a key that is empty or
            // contains '='. An encoder that put the separator in the wrong place,
            // or that let a name carrying '=' through, shows up here.
            if (key.Length == 0) return 4;
            if (key.IndexOf('=') >= 0) return 5;

            // The load-bearing one. GetEnvironmentVariable consults the
            // environment table directly through a different entry point, so this
            // compares the block's idea of each variable against the table's.
            if (Environment.GetEnvironmentVariable(key) != value) return 6;

            seen++;
        }

        // The enumeration really did visit every entry, so a block that ended
        // early cannot pass by simply reporting fewer variables consistently.
        if (seen != vars.Count) return 7;

        // A second call must build a fresh dictionary from a fresh block: the
        // first call frees the block it was given, so a runtime that cached or
        // reused it would be caught here.
        IDictionary again = Environment.GetEnvironmentVariables();

        if (ReferenceEquals(again, vars)) return 8;
        if (again.Count != vars.Count) return 9;

        foreach (DictionaryEntry entry in again)
        {
            string key = (string)entry.Key;

            if (!vars.Contains(key)) return 10;
            if ((string)vars[key] != (string)entry.Value) return 11;
        }

        return 0;
    }
}
