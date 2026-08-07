using System;
using System.Threading;

class Program
{
    // Mirrors System.Threading.CancellationTokenSource.States: a private, Int32-backed enum
    // whose field is updated with Interlocked.Exchange<T>.
    enum States
    {
        NotCanceledState = 0,
        NotifyingState = 1,
        NotifyingCompleteState = 2,
    }

    enum ByteBacked : byte
    {
        Zero = 0,
        Big = 250,
        Other = 9,
    }

    enum ShortBacked : short
    {
        Neg = -1234,
        Pos = 2222,
    }

    enum LongBacked : long
    {
        Min = long.MinValue,
        Ninety = 99L,
        // Shares its low 32 bits with Min, so an exchange that truncated the 64-bit
        // underlying value to 32 bits could not tell the returned old value apart from this.
        LowHalfOfMin = 0L,
    }

    enum UIntBacked : uint
    {
        Max = uint.MaxValue,
        FortyTwo = 42U,
    }

    class Holder
    {
        private volatile States _state;

        public States State => _state;

        public States SetState(States value) => Interlocked.Exchange(ref _state, value);
    }

    static States StaticState = States.NotifyingState;

    static int Main(string[] args)
    {
        // Int32-backed enum in a local. Exchange is unconditional: it always writes, and
        // always returns the value that was there before.
        States s = States.NotCanceledState;
        if (Interlocked.Exchange(ref s, States.NotifyingState) != States.NotCanceledState) return 1;
        if (s != States.NotifyingState) return 2;
        // Exchanging for the value already present still reports the old value and leaves it.
        if (Interlocked.Exchange(ref s, States.NotifyingState) != States.NotifyingState) return 3;
        if (s != States.NotifyingState) return 4;
        if (Interlocked.Exchange(ref s, States.NotifyingCompleteState) != States.NotifyingState) return 5;
        if (s != States.NotifyingCompleteState) return 6;

        // Byte-backed.
        ByteBacked b = ByteBacked.Big;
        if (Interlocked.Exchange(ref b, ByteBacked.Other) != ByteBacked.Big) return 7;
        if (b != ByteBacked.Other) return 8;
        if (Interlocked.Exchange(ref b, ByteBacked.Zero) != ByteBacked.Other) return 9;
        if (b != ByteBacked.Zero) return 10;

        // Short-backed, exercising a negative underlying value in both directions.
        ShortBacked sh = ShortBacked.Neg;
        if (Interlocked.Exchange(ref sh, ShortBacked.Pos) != ShortBacked.Neg) return 11;
        if (sh != ShortBacked.Pos) return 12;
        if (Interlocked.Exchange(ref sh, ShortBacked.Neg) != ShortBacked.Pos) return 13;
        if (sh != ShortBacked.Neg) return 14;

        // 64-bit backing: neither the write nor the returned old value may be truncated to 32 bits.
        LongBacked l = LongBacked.Min;
        if (Interlocked.Exchange(ref l, LongBacked.Ninety) != LongBacked.Min) return 15;
        if (l != LongBacked.Ninety) return 16;
        if (Interlocked.Exchange(ref l, LongBacked.LowHalfOfMin) != LongBacked.Ninety) return 17;
        if (l != LongBacked.LowHalfOfMin) return 18;
        if (Interlocked.Exchange(ref l, LongBacked.Min) != LongBacked.LowHalfOfMin) return 19;
        if (l != LongBacked.Min) return 20;

        // Unsigned 32-bit backing, exercising a value whose signed reading is negative.
        UIntBacked u = UIntBacked.Max;
        if (Interlocked.Exchange(ref u, UIntBacked.FortyTwo) != UIntBacked.Max) return 21;
        if (u != UIntBacked.FortyTwo) return 22;
        if (Interlocked.Exchange(ref u, UIntBacked.Max) != UIntBacked.FortyTwo) return 23;
        if (u != UIntBacked.Max) return 24;

        // Instance field of a heap object, as CancellationTokenSource does it.
        Holder holder = new Holder();
        if (holder.SetState(States.NotifyingCompleteState) != States.NotCanceledState) return 25;
        if (holder.State != States.NotifyingCompleteState) return 26;
        if (holder.SetState(States.NotifyingState) != States.NotifyingCompleteState) return 27;
        if (holder.State != States.NotifyingState) return 28;

        // Static field.
        if (Interlocked.Exchange(ref StaticState, States.NotifyingCompleteState) != States.NotifyingState) return 29;
        if (StaticState != States.NotifyingCompleteState) return 30;

        // Array element: the neighbouring element must be left alone.
        States[] arr = new States[] { States.NotifyingState, States.NotCanceledState };
        if (Interlocked.Exchange(ref arr[1], States.NotifyingCompleteState) != States.NotCanceledState) return 31;
        if (arr[1] != States.NotifyingCompleteState) return 32;
        if (arr[0] != States.NotifyingState) return 33;

        return 0;
    }
}
