#define DEBUG

using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

// Debug.Write reaches SystemNative_SysLog whenever no debugger is logging, which
// under PawPrint is always: DebugProvider.WriteToDebugger hands the message to
// syslog(3) at LOG_DEBUG, with the format "%s". The F# registration asserts the
// exact entries PawPrint recorded, in order.
//
// The raw calls at other levels pin the level to the argument: every
// CoreLib-originated entry is LOG_DEBUG, so without them a handler that ignored
// the priority would pass. The last raw message is not UTF-8: syslog(3) takes
// bytes, and the log records them as given.
//
// PawPrint-only: on a real runtime the messages go to the host's system logger,
// which the guest cannot read back. There is no oracle here.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SysLog")]
    static extern unsafe void SysLog(int priority, byte* message, byte* arg1);

    static void RawSysLog(int priority, string asciiMessage)
    {
        byte[] message = new byte[asciiMessage.Length];
        for (int i = 0; i < asciiMessage.Length; i++)
        {
            message[i] = (byte)asciiMessage[i];
        }

        RawSysLogBytes(priority, message);
    }

    static unsafe void RawSysLogBytes(int priority, byte[] message)
    {
        byte[] format = new byte[] { (byte)'%', (byte)'s', 0 };
        byte[] arg = new byte[message.Length + 1];
        Array.Copy(message, arg, message.Length);

        fixed (byte* f = format)
        fixed (byte* a = arg)
        {
            SysLog(priority, f, a);
        }
    }

    static int Main(string[] args)
    {
        Debug.Write("plain");
        // Ends in a newline, so the next write is indented.
        Debug.WriteLine("line");
        Debug.Indent();
        Debug.Write("indented");
        Debug.Unindent();
        Debug.WriteLine("");
        // DebugProvider.Write sends an empty message for null.
        Debug.Write(null);

        RawSysLog(3, "raw error");
        RawSysLog(0, "raw emergency");
        RawSysLog(6, "raw info");
        RawSysLogBytes(7, new byte[] { (byte)'r', (byte)'a', (byte)'w', (byte)' ', 0xFF, 0xE9 });

        return 0;
    }
}
