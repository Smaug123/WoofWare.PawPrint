using System.Collections.Generic;
using System.Linq;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            var l = new List<int>();
            l.Add(3);
            l.Add(100);
            var m = l.Select(x => x.ToString()).ToList();
            // 2 + 103 + (1 + 3) = 109
            return m.Count + l.Sum() + m.Select(x => x.Length).Sum();
        }
    }
}
