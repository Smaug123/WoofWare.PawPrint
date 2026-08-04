using System;

public class Program
{
    // `calli` interaction with static-constructor timing. (The null-function-pointer case
    // cannot live in a comparison test: the real runtime segfaults on `calli` through
    // null rather than raising a catchable NullReferenceException, so it is covered by a
    // PawPrint-only test in TestPureCases.fs instead.)

    static class WithCctor
    {
        public static int Marker;

        static WithCctor()
        {
            Order.Log(1);
            Marker = 7;
        }

        public static int Get() => Marker;
    }

    static class Order
    {
        public static int Count;
        public static int First;

        public static void Log(int who)
        {
            Count += 1;
            if (First == 0) First = who;
        }
    }

    public static unsafe int Main(string[] args)
    {
        // Taking the address of a method on a type with a static constructor must not by
        // itself run that cctor; the cctor runs no later than the call through it. We
        // record the order so PawPrint and the real runtime are compared on observable
        // sequencing rather than on our guess about it.
        delegate*<int> get = &WithCctor.Get;
        Order.Log(2);
        int got = get();
        if (got != 7) return 2;
        if (Order.Count != 2) return 3;

        // The ldftn happened before Order.Log(2). If the cctor had been forced by ldftn,
        // First would be 1; if it is deferred until the call, First is 2.
        if (Order.First != 2) return 4;

        return 0;
    }
}
