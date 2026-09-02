using System;

// A `void Main` never latches anything on return, so the process exit code is whatever
// the guest last wrote to `Environment.ExitCode`, which starts at 0 and reads back what
// was written. Measured on real .NET 10: exit code 9.
class VoidMainSetsExitCode
{
    static void Main()
    {
        if (Environment.ExitCode != 0)
        {
            Environment.ExitCode = 1;
            return;
        }

        Environment.ExitCode = 4;
        Environment.ExitCode = Environment.ExitCode + 5;
    }
}
