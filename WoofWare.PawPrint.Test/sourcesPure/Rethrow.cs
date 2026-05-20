using System;

namespace RethrowTest
{
    class OuterException : Exception
    {
    }

    class InnerException : Exception
    {
    }

    class Program
    {
        static int SimpleRethrow()
        {
            int marker = 0;

            try
            {
                try
                {
                    throw new OuterException();
                }
                catch
                {
                    marker = 5;
                    throw;
                }
            }
            catch (OuterException)
            {
                return marker == 5 ? 0 : 1;
            }
            catch
            {
                return 2;
            }
        }

        static int FilterRethrow()
        {
            try
            {
                try
                {
                    throw new OuterException();
                }
                catch (OuterException) when (Accept())
                {
                    throw;
                }
            }
            catch (OuterException)
            {
                return 0;
            }
            catch
            {
                return 3;
            }

            return 4;
        }

        static int OuterCatchSurvivesInnerCatch()
        {
            int marker = 0;

            try
            {
                try
                {
                    throw new OuterException();
                }
                catch
                {
                    try
                    {
                        throw new InnerException();
                    }
                    catch (InnerException)
                    {
                        marker = 7;
                    }

                    throw;
                }
            }
            catch (OuterException)
            {
                return marker == 7 ? 0 : 5;
            }
            catch (InnerException)
            {
                return 6;
            }
            catch
            {
                return 8;
            }
        }

        static bool Accept()
        {
            return true;
        }

        static int Main(string[] args)
        {
            int result = SimpleRethrow();
            if (result != 0)
            {
                return 10 + result;
            }

            result = FilterRethrow();
            if (result != 0)
            {
                return 20 + result;
            }

            result = OuterCatchSurvivesInnerCatch();
            if (result != 0)
            {
                return 30 + result;
            }

            return 0;
        }
    }
}
