using System;

class Program
{
    static int Main(string[] args)
    {
        int count = Environment.ProcessorCount;
        if (count > 0)
        {
            return 0;
        }
        return 1;
    }
}
