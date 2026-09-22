using System;
using System.Collections.Generic;

// The width of a value a generic call returns is the call site's: `Id<float>` returns a float32,
// so after it meets a float32 literal at a join the arithmetic stays single precision, and
// `16777216f + 1f + 1f` rounds back to 16777216f at each step. `Id<double>` returns a double, and
// a float32 literal meeting it is widened, so the same additions reach 16777218.
public class Program
{
    private static T Id<T>(T value)
    {
        return value;
    }

    private static float Pick(bool flag, int zero)
    {
        float chosen = flag ? Id<float>(16777216f + zero) : 16777216f + zero;
        return chosen + 1f + 1f;
    }

    private static float FromList(List<float> values, bool flag, int zero)
    {
        float chosen = flag ? values[0] : 16777216f + zero;
        return chosen + 1f + 1f;
    }

    private static double Mixed(bool flag, int zero)
    {
        double chosen = flag ? Id<double>(16777216.0 + zero) : 16777216f + zero;
        return chosen + 1f + 1f;
    }

    public static int Main(string[] args)
    {
        int zero = args.Length;

        if (Pick(true, zero) != 16777216f) return 1;
        if (Pick(false, zero) != 16777216f) return 2;

        List<float> values = new List<float> { 16777216f + zero };
        if (FromList(values, true, zero) != 16777216f) return 3;
        if (FromList(values, false, zero) != 16777216f) return 4;

        if (Mixed(true, zero) != 16777218.0) return 5;
        if (Mixed(false, zero) != 16777218.0) return 6;

        return 0;
    }
}
