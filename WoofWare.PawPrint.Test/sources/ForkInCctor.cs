using System.Threading;

// A guest that forks during *startup*: the entry type's static constructor starts a thread, so
// by the time `Main` is installed there are already two Runnable threads.
//
// `Program.runToFirstFork` must notice this and report `ForkedDuringStartup` rather than
// mistaking startup for a forced prefix. Getting that wrong would be silent: the harness would
// snapshot a state whose prefix already contained a scheduling choice, and every seed resumed
// from it would explore a schedule space missing whatever the startup choice decided.
public class ForkInCctor
{
    private static int flag;

    static ForkInCctor()
    {
        Thread t = new Thread(() => { flag = 1; });
        t.Start();
    }

    private static int Main(string[] args)
    {
        return flag;
    }
}
