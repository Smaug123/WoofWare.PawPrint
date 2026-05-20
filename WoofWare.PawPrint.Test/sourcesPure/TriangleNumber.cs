using System;

namespace TriangleNumber
{
    class Program
    {
        static int Main(string[] args)
        {
            var answer = 0;
            for (int i = 0; i < 5; i++)
            {
                answer += i;
            }
            return answer - 10;
        }
    }
}
