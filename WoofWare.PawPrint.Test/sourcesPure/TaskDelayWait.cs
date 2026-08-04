using System.Threading.Tasks;

// Probed while decomposing issue #713. See TestPureCases.fs's `unimplemented` entry for exactly where
// this fails.
public static class TaskDelayWait
{
    public static int Main(string[] args)
    {
        Task.Delay(1).Wait();
        return 0;
    }
}
