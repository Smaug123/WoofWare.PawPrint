// Thanks Gemini 2.5 Pro

using System;

public class Program
{
    /// <summary>
    /// Main entry point for the test harness. It runs test suites for float and double comparisons.
    /// </summary>
    /// <returns>0 if all tests pass, otherwise a non-zero error code indicating the first failed test.</returns>
    public static int Main(string[] args)
    {
        int result;

        result = FloatCompareTests.RunTests();
        if (result != 0)
        {
            return result;
        }

        result = DoubleCompareTests.RunTests();
        if (result != 0)
        {
            return result;
        }

        return 0; // Success
    }
}

/// <summary>
/// Contains a suite of tests for System.Single (float) comparisons.
/// Each test corresponds to a specific CIL comparison instruction.
/// </summary>
public class FloatCompareTests
{
    private static int testCounter = 100; // Start error codes at 100 for this suite

    /// <summary>
    /// Checks a boolean condition. If the condition is false, it prints a failure message
    /// and returns a unique error code.
    /// </summary>
    /// <param name="condition">The boolean result of the test.</param>
    /// <param name="testName">A descriptive name for the test case.</param>
    /// <returns>0 if the test passes, otherwise a unique non-zero error code.</returns>
    private static int Check(bool condition, string testName)
    {
        testCounter++;
        if (!condition)
        {
            return testCounter;
        }
        return 0;
    }

    /// <summary>
    /// Runs all float comparison tests.
    /// </summary>
    /// <returns>0 if all tests pass, otherwise the error code of the first failing test.</returns>
    public static int RunTests()
    {
        float pz = 0.0f;
        float nz = -0.0f;
        float one = 1.0f;
        float negOne = -1.0f;
        float two = 2.0f;
        float pInf = float.PositiveInfinity;
        float nInf = float.NegativeInfinity;
        float nan = float.NaN;
        float subnormal = BitConverter.ToSingle(new byte[] { 1, 0, 0, 0 }, 0); // Smallest positive subnormal

        int result;

        // --- Ceq Tests (==) ---
        result = Check(one == one, "1.0f == 1.0f"); if (result != 0) return result;
        result = Check(!(one == two), "!(1.0f == 2.0f)"); if (result != 0) return result;
        result = Check(pz == nz, "0.0f == -0.0f"); if (result != 0) return result;
        result = Check(pInf == pInf, "+Inf == +Inf"); if (result != 0) return result;
        result = Check(nInf == nInf, "-Inf == -Inf"); if (result != 0) return result;
        result = Check(!(pInf == nInf), "!(+Inf == -Inf)"); if (result != 0) return result;
        result = Check(!(nan == nan), "!(NaN == NaN)"); if (result != 0) return result;
        result = Check(!(nan == one), "!(NaN == 1.0f)"); if (result != 0) return result;
        result = Check(!(one == nan), "!(1.0f == NaN)"); if (result != 0) return result;

        // --- Cgt Tests (>) ---
        result = Check(two > one, "2.0f > 1.0f"); if (result != 0) return result;
        result = Check(!(one > two), "!(1.0f > 2.0f)"); if (result != 0) return result;
        result = Check(!(one > one), "!(1.0f > 1.0f)"); if (result != 0) return result;
        result = Check(pInf > one, "+Inf > 1.0f"); if (result != 0) return result;
        result = Check(!(nInf > one), "!( -Inf > 1.0f)"); if (result != 0) return result;
        result = Check(pInf > nInf, "+Inf > -Inf"); if (result != 0) return result;
        result = Check(!(nan > one), "!(NaN > 1.0f)"); if (result != 0) return result;
        result = Check(!(one > nan), "!(1.0f > NaN)"); if (result != 0) return result;
        result = Check(one > subnormal, "1.0f > subnormal"); if (result != 0) return result;

        // --- Cgt.un Tests (unordered >) ---
        // cgt.un is equivalent to !(a <= b) for floats
        result = Check(!(two <= one), "cgt.un: 2.0f > 1.0f"); if (result != 0) return result;
        result = Check(one > pz, "cgt.un: 1.0f > 0.0f"); if (result != 0) return result;
        result = Check(!(nan <= one), "cgt.un: NaN > 1.0f"); if (result != 0) return result;
        result = Check(!(one <= nan), "cgt.un: 1.0f > NaN"); if (result != 0) return result;
        result = Check(!(nan <= nan), "cgt.un: NaN > NaN"); if (result != 0) return result;


        // --- Clt Tests (<) ---
        result = Check(one < two, "1.0f < 2.0f"); if (result != 0) return result;
        result = Check(!(two < one), "!(2.0f < 1.0f)"); if (result != 0) return result;
        result = Check(!(one < one), "!(1.0f < 1.0f)"); if (result != 0) return result;
        result = Check(one < pInf, "1.0f < +Inf"); if (result != 0) return result;
        result = Check(nInf < one, "-Inf < 1.0f"); if (result != 0) return result;
        result = Check(nInf < pInf, "-Inf < +Inf"); if (result != 0) return result;
        result = Check(!(nan < one), "!(NaN < 1.0f)"); if (result != 0) return result;
        result = Check(!(one < nan), "!(1.0f < NaN)"); if (result != 0) return result;
        result = Check(subnormal < one, "subnormal < 1.0f"); if (result != 0) return result;

        // --- Clt.un Tests (unordered <) ---
        // clt.un is equivalent to !(a >= b) for floats
        result = Check(one < two, "clt.un: 1.0f < 2.0f"); if (result != 0) return result;
        result = Check(!(one >= nan), "clt.un: 1.0f < NaN"); if (result != 0) return result;
        result = Check(!(nan >= one), "clt.un: NaN < 1.0f"); if (result != 0) return result;
        result = Check(!(nan >= nan), "clt.un: NaN < NaN"); if (result != 0) return result;

        // --- C# >= (bge) and <= (ble) ---
        result = Check(one >= one, "1.0f >= 1.0f"); if (result != 0) return result;
        result = Check(two >= one, "2.0f >= 1.0f"); if (result != 0) return result;
        result = Check(!(nan >= one), "!(NaN >= 1.0f)"); if (result != 0) return result;
        result = Check(one <= one, "1.0f <= 1.0f"); if (result != 0) return result;
        result = Check(one <= two, "1.0f <= 2.0f"); if (result != 0) return result;
        result = Check(!(nan <= one), "!(NaN <= 1.0f)"); if (result != 0) return result;
        result = Check(pz >= nz, "0.0f >= -0.0f"); if (result != 0) return result;
        result = Check(pz <= nz, "0.0f <= -0.0f"); if (result != 0) return result;

        return 0; // Success
    }
}

/// <summary>
/// Contains a suite of tests for System.Double comparisons.
/// </summary>
public class DoubleCompareTests
{
    private static int testCounter = 200; // Start error codes at 200 for this suite

    private static int Check(bool condition, string testName)
    {
        testCounter++;
        if (!condition)
        {
            return testCounter;
        }
        return 0;
    }

    public static int RunTests()
    {
        double pz = 0.0;
        double nz = -0.0;
        double one = 1.0;
        double negOne = -1.0;
        double two = 2.0;
        double pInf = double.PositiveInfinity;
        double nInf = double.NegativeInfinity;
        double nan = double.NaN;
        double subnormal = BitConverter.Int64BitsToDouble(1); // Smallest positive subnormal

        int result;

        // --- Ceq Tests (==) ---
        result = Check(one == one, "1.0 == 1.0"); if (result != 0) return result;
        result = Check(!(one == two), "!(1.0 == 2.0)"); if (result != 0) return result;
        result = Check(pz == nz, "0.0 == -0.0"); if (result != 0) return result;
        result = Check(pInf == pInf, "+Inf == +Inf"); if (result != 0) return result;
        result = Check(nInf == nInf, "-Inf == -Inf"); if (result != 0) return result;
        result = Check(!(pInf == nInf), "!(+Inf == -Inf)"); if (result != 0) return result;
        result = Check(!(nan == nan), "!(NaN == NaN)"); if (result != 0) return result;

        // --- Cgt Tests (>) ---
        result = Check(two > one, "2.0 > 1.0"); if (result != 0) return result;
        result = Check(!(one > one), "!(1.0 > 1.0)"); if (result != 0) return result;
        result = Check(pInf > one, "+Inf > 1.0"); if (result != 0) return result;
        result = Check(!(nInf > one), "!(-Inf > 1.0)"); if (result != 0) return result;
        result = Check(pInf > nInf, "+Inf > -Inf"); if (result != 0) return result;
        result = Check(!(nan > one), "!(NaN > 1.0)"); if (result != 0) return result;
        result = Check(one > subnormal, "1.0 > subnormal"); if (result != 0) return result;

        // --- Cgt.un Tests (unordered >) ---
        result = Check(one > pz, "cgt.un: 1.0 > 0.0"); if (result != 0) return result;
        result = Check(!(nan <= one), "cgt.un: NaN > 1.0"); if (result != 0) return result;
        result = Check(!(one <= nan), "cgt.un: 1.0 > NaN"); if (result != 0) return result;

        // --- Clt Tests (<) ---
        result = Check(one < two, "1.0 < 2.0"); if (result != 0) return result;
        result = Check(!(one < one), "!(1.0 < 1.0)"); if (result != 0) return result;
        result = Check(nInf < one, "-Inf < 1.0"); if (result != 0) return result;
        result = Check(!(pInf < one), "!(+Inf < 1.0)"); if (result != 0) return result;
        result = Check(nInf < pInf, "-Inf < +Inf"); if (result != 0) return result;
        result = Check(!(nan < one), "!(NaN < 1.0)"); if (result != 0) return result;
        result = Check(subnormal < one, "subnormal < 1.0"); if (result != 0) return result;

        // --- Clt.un Tests (unordered <) ---
        result = Check(one < two, "clt.un: 1.0 < 2.0"); if (result != 0) return result;
        result = Check(!(one >= nan), "clt.un: 1.0 < NaN"); if (result != 0) return result;
        result = Check(!(nan >= one), "clt.un: NaN < 1.0"); if (result != 0) return result;

        // --- C# >= (bge) and <= (ble) ---
        result = Check(one >= one, "1.0 >= 1.0"); if (result != 0) return result;
        result = Check(two >= one, "2.0 >= 1.0"); if (result != 0) return result;
        result = Check(!(nan >= one), "!(NaN >= 1.0)"); if (result != 0) return result;
        result = Check(one <= one, "1.0 <= 1.0"); if (result != 0) return result;
        result = Check(one <= two, "1.0 <= 2.0"); if (result != 0) return result;
        result = Check(!(nan <= one), "!(NaN <= 1.0)"); if (result != 0) return result;
        result = Check(pz >= nz, "0.0 >= -0.0"); if (result != 0) return result;

        return 0; // Success
    }
}
