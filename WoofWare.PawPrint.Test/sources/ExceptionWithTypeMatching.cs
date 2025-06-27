using System;

namespace HelloWorld
{
    class Program {
        static int Test_TypeMatching()
        {
            int x = 0;
            try
            {
                try
                {
                    x = 1;
                    throw new ArgumentNullException();  // Derives from ArgumentException
                }
                catch (InvalidOperationException)
                {
                    x = 999;  // Should NOT execute
                }
                catch (ArgumentException)  // This should catch ArgumentNullException
                {
                    x = 2;
                }
                catch (Exception)
                {
                    x = 888;  // Should NOT execute (more general handler)
                }
            }
            catch (Exception)
            {
                x = 777;  // Should NOT execute (outer handler)
            }
            return x;  // Expected: 2
        }

        static int Main(string[] args)
        {
            return Test_TypeMatching();
        }
    }
}
