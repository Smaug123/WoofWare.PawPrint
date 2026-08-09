using System;
using System.Text.RegularExpressions;

public class RegexConstructionRepeatedNonBacktrackingTests
{
    private const string Pattern = "^a(b|c)+d$";

    // The minimal failing shape: *two* NonBacktracking constructions in one process.
    // Either one alone passes (see `RegexConstruction.cs` and
    // `RegexConstructionOptions.cs`); what fails is the second, because
    // `System.Text.RegularExpressions.Symbolic`'s BDD caches are process-wide, so the
    // second regex is the first to find an entry already present and take a cache *hit*.
    public static int TestTwoNonBacktrackingConstructions()
    {
        var first = new Regex(Pattern, RegexOptions.NonBacktracking);
        if (first.Options != RegexOptions.NonBacktracking) return 1;

        const RegexOptions options =
            RegexOptions.NonBacktracking | RegexOptions.Singleline | RegexOptions.IgnoreCase;

        var second = new Regex(Pattern, options);
        if (second.Options != options) return 2;
        if (second.ToString() != Pattern) return 3;

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result = RegexConstructionRepeatedNonBacktrackingTests.TestTwoNonBacktrackingConstructions();
        if (result != 0) return 100 + result;

        return 0;
    }
}
