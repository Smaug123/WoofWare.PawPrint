using System.Collections.Generic;
using System.Linq;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            var l = new List<int>();
            l.Add(8);
            l.Add(100);
            var m = l.Select(x => x.ToString()).ToList();
            // 2 + 108 + (1 + 3) = 114
            var result = m.Count + l.Sum() + m.Select(x => x.Length).Sum();
            if (result == 0)
            {
                return 1;
            }
            if (result != 114)
            {
                return result;
            }

            return 0;
        }
    }
}
