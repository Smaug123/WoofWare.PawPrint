using System;

class Program
{
    static int flag = 0;

    static void HelperWithFinally()
    {
        try
        {
            throw new Exception();
        }
        finally
        {
            flag += 10;
        }
    }

    static int CallerCatchesCrossFrame()
    {
        flag = 0;

        try
        {
            HelperWithFinally();
        }
        catch
        {
            flag += 100;
        }

        return flag;
    }

    static int Main(string[] args)
    {
        if (CallerCatchesCrossFrame() != 110)
        {
            return 1;
        }

        return 0;
    }
}
