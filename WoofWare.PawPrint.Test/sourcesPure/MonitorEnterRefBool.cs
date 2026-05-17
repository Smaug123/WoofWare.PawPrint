using System;
using System.Threading;

/// <summary>
/// Characterization test for Monitor.Enter with ref bool (out-byref parameter).
/// Exercises the pattern where Monitor.Enter writes true to a ref bool argument.
/// </summary>
class MonitorEnterRefBool
{
    static int Main(string[] args)
    {
        object lockObj = new object();
        bool taken = false;
        Monitor.Enter(lockObj, ref taken);
        if (taken) Monitor.Exit(lockObj);
        // taken should be true after Monitor.Enter.
        return taken ? 0 : 1;
    }
}
