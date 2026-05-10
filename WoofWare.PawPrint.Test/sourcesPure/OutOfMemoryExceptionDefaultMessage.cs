using System;

// Constructing OutOfMemoryException with no message argument routes through
// Exception.GetMessageFromNativeResources, which is the QCall
// ExceptionNative_GetMessageFromNativeResources. We assert only that a
// non-empty message comes back, not its exact wording, so the test does not
// pin the runtime's English string.
public class OutOfMemoryExceptionDefaultMessageTests
{
    public static int Main(string[] argv)
    {
        OutOfMemoryException ex = new OutOfMemoryException();
        string message = ex.Message;
        if (message == null) return 1;
        if (message.Length == 0) return 1;
        return 0;
    }
}
