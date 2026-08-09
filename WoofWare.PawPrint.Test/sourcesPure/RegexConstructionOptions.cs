using System;
using System.Text.RegularExpressions;

public class RegexConstructionOptionsTests
{
    private const string Pattern = "^a(b|c)+d$";

    // As `RegexConstruction.cs`, but for `NonBacktracking` combined with the two options
    // that change how the pattern is *interpreted* rather than which engine runs it.
    // `IgnoreCase` in particular drags in the case-equivalence tables, so it exercises
    // considerably more of the symbolic engine's construction path than the bare flag.
    //
    // This lives in its own file rather than beside the other constructions because a
    // second NonBacktracking construction in the same process does not work yet; see
    // `RegexConstructionRepeatedNonBacktracking.cs`.
    public static int TestNonBacktrackingWithFlags()
    {
        const RegexOptions options =
            RegexOptions.NonBacktracking | RegexOptions.Singleline | RegexOptions.IgnoreCase;

        var r = new Regex(Pattern, options);

        if (r == null) return 1;
        if (r.ToString() != Pattern) return 2;
        if (r.Options != options) return 3;
        if (r.RightToLeft) return 4;
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result = RegexConstructionOptionsTests.TestNonBacktrackingWithFlags();
        if (result != 0) return 100 + result;

        return 0;
    }
}
