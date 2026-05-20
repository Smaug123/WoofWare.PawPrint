using System.Collections.Generic;

namespace EqualityComparerDefaultStringTest
{
    class Program
    {
        static int Main(string[] args)
        {
            return EqualityComparer<string>.Default == null ? 1 : 0;
        }
    }
}
