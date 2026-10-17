using System.Threading;

// Several threads find the lock held while its owner sleeps, so each spins past the thin lock and
// inflates it to a Lock at much the same time: one of them inflates it, handing it the sleeping
// owner, and the rest wait for that before queueing on the same Lock. Every one of them then
// enters it, once the owner leaves.
class Program
{
    static readonly object Gate = new object();
    static int entered;

    static int Main()
    {
        Thread[] contenders = new Thread[3];

        lock (Gate)
        {
            for (int i = 0; i < contenders.Length; i++)
            {
                contenders[i] = new Thread(() =>
                {
                    lock (Gate)
                    {
                        entered++;
                    }
                });
                contenders[i].Start();
            }

            Thread.Sleep(50);

            if (entered != 0) return 1;
        }

        foreach (Thread t in contenders) t.Join();

        return entered == contenders.Length ? 0 : 2;
    }
}
