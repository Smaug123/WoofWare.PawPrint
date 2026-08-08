using System.Threading.Tasks;

// Probed while decomposing issue #713, and parked at the time: reaching a timer at all needs the
// `SystemNative_SchedGetCpu` PInvoke (#724) and `conv.ovf.i` (#753), which `TimerQueueTimer..ctor`
// uses to index `TimerQueue.Instances`, and then `System.Threading.Lock.ThreadId` needs the OS
// thread id PAL entry points. Those landed in #742, #756 and #768 respectively, so this case has
// passed since #768 and is no longer in TestPureCases.fs's `unimplemented` set.
public static class TaskDelayWait
{
    public static int Main(string[] args)
    {
        Task.Delay(1).Wait();
        return 0;
    }
}
