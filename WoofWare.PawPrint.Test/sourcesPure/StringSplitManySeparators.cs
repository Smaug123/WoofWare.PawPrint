using System;

// More than 3 distinct separator chars routes String.Split through the ProbabilisticMap
// path (MakeSeparatorListAny's `else` branch), rather than the <=3-separator manual
// comparison or the whitespace special case.
public class TestStringSplitManySeparators
{
    public static int Main(string[] argv)
    {
        char[] seps = new char[] { '/', ',', ';', ':', '|' };
        string[] parts = "a/b,c;d:e|f".Split(seps);

        if (parts.Length != 6) return 1;
        if (parts[0] != "a") return 2;
        if (parts[5] != "f") return 3;

        return 0;
    }
}
