using System;
using System.Reflection;

// `AppDomain.AssemblyResolve`, which CoreCLR raises from the binder when a request finds nothing,
// and whose answer then stands as the load's result. A handler may answer with any assembly.
public class Program
{
    public static int Main (string[] args)
    {
        string asked = null;

        AppDomain.CurrentDomain.AssemblyResolve += (sender, e) =>
        {
            asked = e.Name;
            return typeof (object).Assembly;
        };

        Assembly answered = Assembly.Load ("No.Such.Assembly");
        if (!ReferenceEquals (answered, typeof (object).Assembly))
            return 1;
        // The handler is handed the requested display name, not the bare simple name.
        if (asked != "No.Such.Assembly, Culture=neutral, PublicKeyToken=null")
            return 2;

        return 0;
    }
}
