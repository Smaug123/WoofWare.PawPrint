using System.Runtime.InteropServices;

class Program
{
    static int Main(string[] args)
    {
        Marshal.SetLastPInvokeError(1234);
        return Marshal.GetLastPInvokeError() == 1234 ? 0 : 1;
    }
}
