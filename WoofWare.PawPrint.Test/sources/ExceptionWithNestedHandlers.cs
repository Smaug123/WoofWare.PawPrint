using System;

namespace HelloWorld
{
    class Program {
        static int Test_NestedHandlers()
        {
            int x = 0;
            try
            {
                try
                {
                    x = 1;
                    throw new InvalidOperationException();
                }
                catch (InvalidOperationException)
                {
                    x = 2;
                    throw new ArgumentException(); // Re-throw different exception
                }
                finally
                {
                    x += 10; // Inner finally
                }
            }
            catch (ArgumentException)
            {
                x += 100; // Outer catch
            }
            catch (Exception)
            {
                x += 1000; // This should NOT execute
            }
            finally
            {
                x += 10000; // Outer finally
            }

            return x; // Expected: 10112 (1 -> 2 -> 12 -> 112 -> 10112)
        }

        static int Main(string[] args)
        {
            return Test_NestedHandlers();
        }
    }
}
