using System;
using System.Collections.Generic;

// A generic method's return type at a call site is what the call site binds it to: `Id<float>`
// returns a float32, so its result may meet a float32 literal at a join and then carry on in
// float32 arithmetic. Likewise a generic type's member: `List<float>[i]` is a float32. The values
// are small enough that no rounding is involved: what is checked is that these shapes are
// accepted and run, not the width of the arithmetic after the join.
public class Program
{
    private static T Id<T>(T value)
    {
        return value;
    }

    private static float Pick(bool flag, int zero)
    {
        float chosen = flag ? Id<float>(4f + zero) : 4f + zero;
        return chosen + 1f + 1f;
    }

    private static float FromList(List<float> values, bool flag, int zero)
    {
        float chosen = flag ? values[0] : 4f + zero;
        return chosen + 1f + 1f;
    }

    private static double Mixed(bool flag, int zero)
    {
        // A generic double meeting a float32 literal: C# widens the literal, and the sum is double.
        double chosen = flag ? Id<double>(4.0 + zero) : 4f + zero;
        return chosen + 1f + 1f;
    }

    public static int Main(string[] args)
    {
        int zero = args.Length;

        if (Pick(true, zero) != 6f) return 1;
        if (Pick(false, zero) != 6f) return 2;

        List<float> values = new List<float> { 4f + zero };
        if (FromList(values, true, zero) != 6f) return 3;
        if (FromList(values, false, zero) != 6f) return 4;

        if (Mixed(true, zero) != 6.0) return 5;
        if (Mixed(false, zero) != 6.0) return 6;

        return 0;
    }
}
