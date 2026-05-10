using System;
using System.Threading;

class Program
{
    class Holder
    {
        public bool Flag = false;
    }

    static bool s_staticFlag = false;

    static int Main(string[] args)
    {
        bool local = false;
        if (Interlocked.Exchange(ref local, true) != false || local != true) return 1;
        if (Interlocked.Exchange(ref local, false) != true || local != false) return 2;
        if (Interlocked.Exchange(ref local, false) != false || local != false) return 3;
        if (Interlocked.Exchange(ref local, true) != false || local != true) return 4;

        Holder h = new Holder();
        if (Interlocked.Exchange(ref h.Flag, true) != false || h.Flag != true) return 5;
        if (Interlocked.Exchange(ref h.Flag, false) != true || h.Flag != false) return 6;

        if (Interlocked.Exchange(ref s_staticFlag, true) != false || s_staticFlag != true) return 7;
        if (Interlocked.Exchange(ref s_staticFlag, true) != true || s_staticFlag != true) return 8;

        // CompareExchange(ref bool, bool, bool) rides the same scalar arm; verify it.
        bool cas = false;
        if (Interlocked.CompareExchange(ref cas, true, true) != false || cas != false) return 9;
        if (Interlocked.CompareExchange(ref cas, true, false) != false || cas != true) return 10;
        if (Interlocked.CompareExchange(ref cas, false, false) != true || cas != true) return 11;
        if (Interlocked.CompareExchange(ref cas, false, true) != true || cas != false) return 12;

        return 0;
    }
}
