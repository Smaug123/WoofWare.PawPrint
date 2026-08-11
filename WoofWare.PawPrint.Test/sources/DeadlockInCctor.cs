using System.Threading;

// The same wedge as `DeadlockBeforeFork.cs`, but during class initialisation rather than in
// `Main`. `runToFirstFork` drives startup through its own loop, so the startup and main-phase
// deadlock arms are separate code paths and a guest that only exercises one leaves the other
// free to rot.
public class DeadlockInCctor
{
    private static int flag;

    static DeadlockInCctor()
    {
        Thread.Sleep(Timeout.Infinite);
        flag = 1;
    }

    private static int Main(string[] args)
    {
        return flag;
    }
}
