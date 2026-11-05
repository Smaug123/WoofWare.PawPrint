using System;
using System.Net;

// The machine's host name, as the guest sees it through Dns.GetHostName and
// Environment.MachineName, both of which read SystemNative_GetHostName.
//
// Impure rather than pure because the value is PawPrint's own: the real
// runtime reports the name of whatever machine runs the test. Hardcodes no
// name: it prints both to stdout, a line each, so the F# registration, which
// configured KernelConfig.HostName, asserts the exact bytes.
public class TestHostNameConfigured
{
    public static int Main(string[] argv)
    {
        string dns = Dns.GetHostName();
        string machine = Environment.MachineName;

        // MachineName is the host name up to its first dot, on every Unix.
        int dot = dns.IndexOf('.');
        if (machine != (dot < 0 ? dns : dns.Substring(0, dot))) return 1;

        Console.Out.Write(dns + "\n" + machine + "\n");
        return 0;
    }
}
