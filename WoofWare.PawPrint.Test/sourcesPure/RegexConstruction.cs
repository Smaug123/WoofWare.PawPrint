using System;
using System.Text.RegularExpressions;

public class RegexConstructionTests
{
    private const string Pattern = "^a(b|c)+d$";

    // Construction only: each case builds a Regex and inspects the properties the
    // constructor itself is responsible for setting. `ToString()` returns the pattern
    // and `Options` returns exactly the flags passed in (the constructor does not
    // normalise or add any), so both are pure statements about construction rather
    // than about the matching engine.
    private static int Check(Regex r, RegexOptions expectedOptions)
    {
        if (r == null) return 1;
        if (r.ToString() != Pattern) return 2;
        if (r.Options != expectedOptions) return 3;
        if (r.RightToLeft) return 4;
        return 0;
    }

    public static int TestDefault()
    {
        var r = new Regex(Pattern);
        return Check(r, RegexOptions.None);
    }

    public static int TestNonBacktracking()
    {
        var r = new Regex(Pattern, RegexOptions.NonBacktracking);
        return Check(r, RegexOptions.NonBacktracking);
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = RegexConstructionTests.TestDefault();
        if (result != 0) return 100 + result;

        // Exactly one NonBacktracking construction. A *second* one in the same process
        // takes a hit in a process-wide BDD cache and does not work yet; that is
        // `RegexConstructionRepeatedNonBacktracking.cs`, which is parked.
        result = RegexConstructionTests.TestNonBacktracking();
        if (result != 0) return 200 + result;

        return 0;
    }
}
