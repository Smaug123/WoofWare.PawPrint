using System;

namespace ExceptionTests
{
    class Program
    {
        // Test 1: Basic try-catch
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
            return x; // Expected: 123 (loop 0: 0+1+10=11, loop 1: 11+1+100=112, loop 2: 112+1+10=123)
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
            int result;

            result = Test1_BasicCatch();
            if (result != 4) return 1000000 + result;

            result = Test2_NoException();
            if (result != 20) return 2000000 + result;

            result = Test3_FinallyAlwaysRuns();
            if (result != 115) return 3000000 + result;

            result = Test4_FinallyNoException();
            if (result != 12) return 4000000 + result;

            result = Test5_SpecificException();
            if (result != 1) return 5000000 + result;

            result = Test6_BaseExceptionCatch();
            if (result != 10) return 6000000 + result;

            result = Test7_ExceptionFallthrough();
            if (result != 2) return 7000000 + result;

            result = Test8_NestedTryCatch();
            if (result != 112) return 8000000 + result;

            result = Test9_ExceptionInFinally();
            if (result != 12) return 9000000 + result;

            result = Test10_MultipleFinallyBlocks();
            if (result != 1111) return 10000000 + result;

            result = Test11_MethodException();
            if (result != 42) return 11000000 + result;

            result = Test12_ExceptionPropagation();
            if (result != 99) return 12000000 + result;

            result = Test13_Rethrow();
            if (result != 25) return 13000000 + result;

            result = Test14_TryFinallyNoCatch();
            if (result != 50) return 14000000 + result;

            result = Test15_ComplexFlow();
            if (result != 123) return 15000000 + result;

            result = Test16_ConditionalFlow();
            if (result != 20) return 16000000 + result;

            result = Test17_ExceptionInCatch();
            if (result != 35) return 17000000 + result;

            result = Test18_Wrapper();
            if (result != 5) return 18000000 + result;

            result = Test19_DeepNesting();
            if (result != 11111) return 19000000 + result;

            result = Test20_ExceptionHierarchy();
            if (result != 100001) return 20000000 + result;

            return 0;
        }
    }
}
