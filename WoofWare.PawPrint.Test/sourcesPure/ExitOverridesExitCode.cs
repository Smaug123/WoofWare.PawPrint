using System;

// `Environment.Exit(n)` latches `n` before it tears the process down, so it overrides
// an earlier `Environment.ExitCode` write. Measured on real .NET 10: exit code 2.
class ExitOverridesExitCode
{
    static void Main()
    {
        Environment.ExitCode = 5;
        Environment.Exit(2);
    }
}
