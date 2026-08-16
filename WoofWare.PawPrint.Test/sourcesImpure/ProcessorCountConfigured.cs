using System;

// Impure because it asserts a *specific* processor count, which only holds under
// PawPrint's configured kernel: the real CLR would report this machine's core
// count, so there is no cross-runtime oracle for it.
namespace HelloWorldApp
{
    class Program
    {
        // Read during static initialisation, which is what this test pins.
        // CoreLib declares `Environment.ProcessorCount` as
        // `public static int ProcessorCount { get; } = GetProcessorCount()`, so the
        // first read latches the value into a static for the life of the process --
        // and PawPrint pumps this type's .cctor inside `Program.prepare`. If the
        // host's configured count were applied any later than it is, this field
        // would capture the default instead and the test would fail here rather
        // than in Main.
        static readonly int CountFromStaticInitializer = Environment.ProcessorCount;

        static int Main(string[] args)
        {
            if (CountFromStaticInitializer != 4)
            {
                return 1;
            }

            if (Environment.ProcessorCount != 4)
            {
                return 2;
            }

            return 0;
        }
    }
}
