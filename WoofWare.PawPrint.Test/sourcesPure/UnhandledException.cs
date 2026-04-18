using System;

namespace UnhandledExceptionTest
{
    class Program
    {
        static int Main(string[] args)
        {
            throw new InvalidOperationException("This exception is intentionally unhandled.");
        }
    }
}
