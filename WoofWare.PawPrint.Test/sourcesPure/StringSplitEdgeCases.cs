using System;
using System.Text;

// Two edge cases that push String.Split off its "small input" happy path:
//   1. The parameterless Split() overload, which treats any whitespace as a separator
//      (MakeSeparatorListAny's separators.Length == 0 branch).
//   2. A string with more separator occurrences than StackallocIntBufferSizeLimit (128),
//      forcing ValueListBuilder<int> to grow via ArrayPool<int>.Shared.Rent instead of
//      staying inside its stackalloc'd buffer.
public class TestStringSplitEdgeCases
{
    public static int Main(string[] argv)
    {
        string[] whitespaceSplit = "the quick brown\tfox".Split();
        if (whitespaceSplit.Length != 4) return 1;
        if (whitespaceSplit[0] != "the") return 2;
        if (whitespaceSplit[3] != "fox") return 3;

        var sb = new StringBuilder();
        for (int i = 0; i < 200; i++)
        {
            if (i > 0) sb.Append('/');
            sb.Append(i);
        }

        string[] many = sb.ToString().Split('/');
        if (many.Length != 200) return 4;
        if (many[0] != "0") return 5;
        if (many[199] != "199") return 6;
        if (many[128] != "128") return 7;

        return 0;
    }
}
