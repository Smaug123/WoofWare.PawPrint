using System.Threading;

// `Thread.ThreadState` read from another thread across a thread's lifecycle and across each
// kind of wait. CoreCLR reports `WaitSleepJoin` only for an alertable wait (`TS_Interruptible`,
// comsynchronizable.cpp), so blocking on a class initialiser reads as `Running` although the
// thread is not running. Every comparison is exact, so an extra bit fails as surely as a
// missing one. A failing check still releases every worker it left blocked, so the guest
// reports that check's number rather than waiting for ever on a foreground thread.
class ThreadStateObserved
{
    static readonly ManualResetEvent Evt = new ManualResetEvent(false);
    static readonly object Gate = new object();

    static class InitGate
    {
        public static readonly ManualResetEvent Release = new ManualResetEvent(false);
        public static volatile bool Entered;
    }

    static class SlowInit
    {
        static SlowInit()
        {
            InitGate.Entered = true;
            InitGate.Release.WaitOne();
        }

        public static int X;
    }

    static ThreadState AwaitWaitSleepJoin(Thread t)
    {
        ThreadState s = t.ThreadState;
        while ((s & ThreadState.WaitSleepJoin) == 0)
        {
            Thread.Yield();
            s = t.ThreadState;
        }
        return s;
    }

    static Thread Start(ThreadStart body, bool background)
    {
        Thread t = new Thread(body);
        t.IsBackground = background;
        t.Start();
        return t;
    }

    static int Main(string[] args)
    {
        int result = Run();
        Evt.Set();
        lock (Gate)
        {
            Monitor.PulseAll(Gate);
        }
        InitGate.Release.Set();
        return result;
    }

    static int Run()
    {
        Thread unstarted = new Thread(() => { });
        if (unstarted.ThreadState != ThreadState.Unstarted)
        {
            return 1;
        }
        unstarted.IsBackground = true;
        if (unstarted.ThreadState != (ThreadState.Unstarted | ThreadState.Background))
        {
            return 2;
        }

        if (Thread.CurrentThread.ThreadState != ThreadState.Running)
        {
            return 3;
        }

        ThreadState seenBySelf = ThreadState.Stopped;
        Thread foreground = Start(() => { seenBySelf = Thread.CurrentThread.ThreadState; }, false);
        foreground.Join();
        if (seenBySelf != ThreadState.Running)
        {
            return 4;
        }
        if (foreground.ThreadState != ThreadState.Stopped)
        {
            return 5;
        }

        Thread background = Start(() => { seenBySelf = Thread.CurrentThread.ThreadState; }, true);
        background.Join();
        if (seenBySelf != ThreadState.Background)
        {
            return 6;
        }
        // A dead thread loses its Background bit: CoreCLR clears TS_Background on death.
        if (background.ThreadState != ThreadState.Stopped)
        {
            return 7;
        }

        // Never woken; being background, it does not hold the process open.
        Thread sleeper = Start(() => Thread.Sleep(Timeout.Infinite), true);
        if (AwaitWaitSleepJoin(sleeper) != (ThreadState.WaitSleepJoin | ThreadState.Background))
        {
            return 8;
        }

        Thread waiter = Start(() => Evt.WaitOne(), false);
        if (AwaitWaitSleepJoin(waiter) != ThreadState.WaitSleepJoin)
        {
            return 9;
        }
        Thread joiner = Start(() => waiter.Join(), false);
        if (AwaitWaitSleepJoin(joiner) != ThreadState.WaitSleepJoin)
        {
            return 10;
        }
        ManualResetEvent never = new ManualResetEvent(false);
        Thread anyWaiter = Start(() => WaitHandle.WaitAny(new WaitHandle[] { Evt, never }), false);
        if (AwaitWaitSleepJoin(anyWaiter) != ThreadState.WaitSleepJoin)
        {
            return 11;
        }
        Evt.Set();
        waiter.Join();
        joiner.Join();
        anyWaiter.Join();

        Thread monitorWaiter = Start(() =>
        {
            lock (Gate)
            {
                Monitor.Wait(Gate);
            }
        }, false);
        if (AwaitWaitSleepJoin(monitorWaiter) != ThreadState.WaitSleepJoin)
        {
            return 12;
        }
        lock (Gate)
        {
            Monitor.Pulse(Gate);
        }
        monitorWaiter.Join();

        Thread contender;
        lock (Gate)
        {
            contender = Start(() =>
            {
                lock (Gate)
                {
                }
            }, false);
            if (AwaitWaitSleepJoin(contender) != ThreadState.WaitSleepJoin)
            {
                return 13;
            }
        }
        contender.Join();

        Thread initRunner = Start(() => { SlowInit.X = 1; }, false);
        while (!InitGate.Entered)
        {
            Thread.Yield();
        }
        Thread initBlocked = Start(() => { SlowInit.X = 2; }, false);
        // Nothing distinguishes "blocked on the initialiser" from "not there yet" through
        // `ThreadState`, which is the point: both read `Running`. The sleep gives the worker
        // time to reach the initialiser.
        Thread.Sleep(50);
        if (initBlocked.ThreadState != ThreadState.Running)
        {
            return 14;
        }
        if (AwaitWaitSleepJoin(initRunner) != ThreadState.WaitSleepJoin)
        {
            return 15;
        }
        InitGate.Release.Set();
        initRunner.Join();
        initBlocked.Join();

        return 0;
    }
}
