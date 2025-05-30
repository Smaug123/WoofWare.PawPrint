using System;

namespace ExceptionTests
{
    class Program
    {
        // Test 1: Basic try-catch (your example)
        static int Test1_BasicCatch()
        {
            int x = 3;
            try
            {
                throw new Exception("hello");
            }
            catch
            {
                x += 1;
            }
            return x; // Expected: 4
        }

        // Test 2: No exception thrown
        static int Test2_NoException()
        {
            int x = 10;
            try
            {
                x = x * 2;
            }
            catch
            {
                x = -1;
            }
            return x; // Expected: 20
        }

        // Test 3: Finally block always executes
        static int Test3_FinallyAlwaysRuns()
        {
            int x = 0;
            try
            {
                x = 5;
                throw new Exception();
            }
            catch
            {
                x += 10;
            }
            finally
            {
                x += 100;
            }
            return x; // Expected: 115
        }

        // Test 4: Finally without exception
        static int Test4_FinallyNoException()
        {
            int x = 1;
            try
            {
                x = 2;
            }
            finally
            {
                x += 10;
            }
            return x; // Expected: 12
        }

        // Test 5: Specific exception type caught
        static int Test5_SpecificException()
        {
            int x = 0;
            try
            {
                throw new ArgumentException();
            }
            catch (ArgumentException)
            {
                x = 1;
            }
            catch (Exception)
            {
                x = 2;
            }
            return x; // Expected: 1
        }

        // Test 6: Base exception caught when derived thrown
        static int Test6_BaseExceptionCatch()
        {
            int x = 0;
            try
            {
                throw new ArgumentNullException();
            }
            catch (ArgumentException) // ArgumentNullException derives from ArgumentException
            {
                x = 10;
            }
            catch (Exception)
            {
                x = 20;
            }
            return x; // Expected: 10
        }

        // Test 7: Exception not caught by specific handler
        static int Test7_ExceptionFallthrough()
        {
            int x = 0;
            try
            {
                try
                {
                    throw new InvalidOperationException();
                }
                catch (ArgumentException)
                {
                    x = 1;
                }
            }
            catch (InvalidOperationException)
            {
                x = 2;
            }
            return x; // Expected: 2
        }

        // Test 8: Nested try-catch
        static int Test8_NestedTryCatch()
        {
            int x = 0;
            try
            {
                x = 1;
                try
                {
                    x = 2;
                    throw new Exception("inner");
                }
                catch
                {
                    x += 10;
                    throw new Exception("rethrow");
                }
            }
            catch
            {
                x += 100;
            }
            return x; // Expected: 112
        }

        // Test 9: Exception in finally block
        static int Test9_ExceptionInFinally()
        {
            int x = 0;
            try
            {
                try
                {
                    x = 1;
                }
                finally
                {
                    x = 2;
                    throw new Exception("finally");
                }
            }
            catch
            {
                x += 10;
            }
            return x; // Expected: 12
        }

        // Test 10: Multiple finally blocks in nested structure
        static int Test10_MultipleFinallyBlocks()
        {
            int x = 0;
            try
            {
                try
                {
                    x = 1;
                    throw new Exception();
                }
                finally
                {
                    x += 10;
                }
            }
            catch
            {
                x += 100;
            }
            finally
            {
                x += 1000;
            }
            return x; // Expected: 1111
        }

        // Test 11: Method call that throws
        static int ThrowingMethod()
        {
            throw new Exception("method exception");
        }

        static int Test11_MethodException()
        {
            int x = 0;
            try
            {
                x = ThrowingMethod();
            }
            catch
            {
                x = 42;
            }
            return x; // Expected: 42
        }

        // Test 12: Exception propagation through methods
        static int MiddleMethod()
        {
            return ThrowingMethod() + 10;
        }

        static int Test12_ExceptionPropagation()
        {
            int x = 0;
            try
            {
                x = MiddleMethod();
            }
            catch
            {
                x = 99;
            }
            return x; // Expected: 99
        }

        // Test 13: Re-throw preserves stack
        static int Test13_Rethrow()
        {
            int x = 0;
            try
            {
                try
                {
                    throw new Exception();
                }
                catch
                {
                    x = 5;
                    throw; // Re-throw same exception
                }
            }
            catch
            {
                x += 20;
            }
            return x; // Expected: 25
        }

        // Test 14: Try-finally without catch (exception propagates)
        static int HelperForTest14()
        {
            int x = 0;
            try
            {
                x = 1;
                throw new Exception();
            }
            finally
            {
                x = 10;
            }
            return x; // Won't reach here
        }

        static int Test14_TryFinallyNoCatch()
        {
            int result = 0;
            try
            {
                result = HelperForTest14();
            }
            catch
            {
                result = 50;
            }
            return result; // Expected: 50
        }

        // Test 15: Complex control flow
        static int Test15_ComplexFlow()
        {
            int x = 0;
            for (int i = 0; i < 3; i++)
            {
                try
                {
                    x += 1;
                    if (i == 1)
                        throw new Exception();
                    x += 10;
                }
                catch
                {
                    x += 100;
                }
            }
            return x; // Expected: 222 (loop 0: 1+10=11, loop 1: 11+1+100=112, loop 2: 112+1+10=123... wait)
                      // Actually: loop 0: 0+1+10=11, loop 1: 11+1+100=112, loop 2: 112+1+10=123
        }

        // Test 16: Exception with conditional catch
        static int Test16_ConditionalFlow()
        {
            int x = 0;
            bool caught = false;
            try
            {
                x = 10;
                throw new ArgumentException();
            }
            catch (ArgumentException) when (x > 5) // C# 6 feature - exception filters
            {
                caught = true;
                x = 20;
            }
            catch
            {
                x = 30;
            }
            return caught ? x : -1; // Expected: 20
        }

        // Test 17: Exception in catch block
        static int Test17_ExceptionInCatch()
        {
            int x = 0;
            try
            {
                try
                {
                    throw new InvalidOperationException();
                }
                catch (InvalidOperationException)
                {
                    x = 10;
                    throw new ArgumentException();
                }
            }
            catch (ArgumentException)
            {
                x += 25;
            }
            return x; // Expected: 35
        }

        // Test 18: Finally executes even with return
        static int Test18_FinallyWithReturn()
        {
            int x = 0;
            try
            {
                x = 5;
                return x;
            }
            finally
            {
                x = 10; // This changes x but doesn't affect return value
            }
        }

        static int Test18_Wrapper()
        {
            int result = Test18_FinallyWithReturn();
            return result; // Expected: 5 (not 10!)
        }

        // Test 19: Multiple nested finally blocks with exception
        static int Test19_DeepNesting()
        {
            int x = 0;
            try
            {
                try
                {
                    try
                    {
                        x = 1;
                        throw new Exception("deep");
                    }
                    finally
                    {
                        x += 10;
                    }
                }
                finally
                {
                    x += 100;
                }
            }
            catch
            {
                x += 1000;
            }
            finally
            {
                x += 10000;
            }
            return x; // Expected: 11111
        }

        // Test 20: Exception type hierarchies
        static int Test20_ExceptionHierarchy()
        {
            int x = 0;

            // First exception
            try
            {
                throw new ArgumentNullException();
            }
            catch (ArgumentNullException)
            {
                x += 1;
            }
            catch (ArgumentException)
            {
                x += 10;
            }
            catch (SystemException)
            {
                x += 100;
            }
            catch (Exception)
            {
                x += 1000;
            }

            // Second exception
            try
            {
                throw new SystemException();
            }
            catch (ArgumentException)
            {
                x += 10000;
            }
            catch (SystemException)
            {
                x += 100000;
            }

            return x; // Expected: 100001
        }

        static int Main(string[] args)
        {
            var x1 = Test1_BasicCatch();
            var x2 = Test2_NoException();
            var x3 = Test3_FinallyAlwaysRuns();
            var x4 = Test4_FinallyNoException();
            var x5 = Test5_SpecificException();
            var x6 = Test6_BaseExceptionCatch();
            var x7 = Test7_ExceptionFallthrough();
            var x8 = Test8_NestedTryCatch();
            var x9 = Test9_ExceptionInFinally();
            var x10 = Test10_MultipleFinallyBlocks();
            var x11 = Test11_MethodException();
            var x12 = Test12_ExceptionPropagation();
            var x13 = Test13_Rethrow();
            var x14 = Test14_TryFinallyNoCatch();
            var x15 = Test15_ComplexFlow();
            var x16 = Test16_ConditionalFlow();
            var x17 = Test17_ExceptionInCatch();
            var x18 = Test18_FinallyWithReturn();
            var x19 = Test19_DeepNesting();
            var x20 = Test20_ExceptionHierarchy();

            return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 +
                   x19 + x20;
        }
    }
}
