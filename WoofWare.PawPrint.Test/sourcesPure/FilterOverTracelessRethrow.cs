using System;
using System.Reflection;

// A rethrow of an exception whose trace a handler cleared reaches a filter in the same method with
// no frames: the rethrow appends none for the frame it begins in, so the filter is offered an
// exception that has not been given a trace.

public static class Program
{
    public static int Main(string[] args)
    {
        FieldInfo stackTrace = typeof(Exception).GetField("_stackTrace", BindingFlags.NonPublic | BindingFlags.Instance);
        FieldInfo stackTraceString = typeof(Exception).GetField("_stackTraceString", BindingFlags.NonPublic | BindingFlags.Instance);

        bool filtered = false;
        try
        {
            try
            {
                throw new InvalidOperationException("cleared");
            }
            catch (InvalidOperationException e)
            {
                stackTrace.SetValue(e, null);
                stackTraceString.SetValue(e, null);
                throw;
            }
        }
        catch (InvalidOperationException) when (filtered = true)
        {
        }

        return filtered ? 0 : 1;
    }
}
