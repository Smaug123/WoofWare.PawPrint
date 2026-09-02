using System;

// `Environment.ExitCode` and an `int Main`'s return value are the same latched value.
// `RunMain` (assembly.cpp) writes the return value to the latch immediately after Main
// returns, so it overwrites whatever Main latched through the property: the process
// exits 3, not 5. Measured on real .NET 10: exit code 3.
class IntMainReturnOverridesExitCode
{
    static int Main()
    {
        Environment.ExitCode = 5;
        return 3;
    }
}
