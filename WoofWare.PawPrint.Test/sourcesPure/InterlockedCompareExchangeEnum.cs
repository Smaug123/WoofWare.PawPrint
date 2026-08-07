using System;
using System.Threading;

class Program
{
    // Mirrors System.Threading.CancellationTokenSource.States: a private, Int32-backed enum
    // whose field is updated with Interlocked.CompareExchange<T>.
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
        Other = 7,
    }

    enum LongBacked : long
    {
        Min = long.MinValue,
        Ninety = 99L,
        // Shares its low 32 bits with Min, so a comparison that truncates the 64-bit
        // underlying value to 32 bits would wrongly treat this as equal to Min.
        LowHalfOfMin = 0L,
    }

    enum UIntBacked : uint
    {
        Max = uint.MaxValue,
        FortyTwo = 42U,
        Other = 41U,
    }

    class Holder
    {
        private volatile States _state;

        public States State => _state;

        public States TrySetState(States value, States comparand) =>
            Interlocked.CompareExchange(ref _state, value, comparand);
    }

    static States StaticState = States.NotifyingState;

    static int Main(string[] args)
    {
        // Int32-backed enum in a local.
        States s = States.NotCanceledState;
        if (Interlocked.CompareExchange(ref s, States.NotifyingState, States.NotifyingCompleteState) != States.NotCanceledState) return 1;
        if (s != States.NotCanceledState) return 2;
        if (Interlocked.CompareExchange(ref s, States.NotifyingState, States.NotCanceledState) != States.NotCanceledState) return 3;
        if (s != States.NotifyingState) return 4;

        // Byte-backed: the comparand must be compared at the underlying width, not widened.
        ByteBacked b = ByteBacked.Big;
        if (Interlocked.CompareExchange(ref b, ByteBacked.Other, ByteBacked.Zero) != ByteBacked.Big) return 5;
        if (b != ByteBacked.Big) return 6;
        if (Interlocked.CompareExchange(ref b, ByteBacked.Other, ByteBacked.Big) != ByteBacked.Big) return 7;
        if (b != ByteBacked.Other) return 8;

        // Short-backed, exercising a negative underlying value.
        ShortBacked sh = ShortBacked.Neg;
        if (Interlocked.CompareExchange(ref sh, ShortBacked.Pos, ShortBacked.Other) != ShortBacked.Neg) return 9;
        if (sh != ShortBacked.Neg) return 10;
        if (Interlocked.CompareExchange(ref sh, ShortBacked.Pos, ShortBacked.Neg) != ShortBacked.Neg) return 11;
        if (sh != ShortBacked.Pos) return 12;

        // 64-bit backing: must not be truncated to 32 bits.
        LongBacked l = LongBacked.Min;
        if (Interlocked.CompareExchange(ref l, LongBacked.Ninety, LongBacked.LowHalfOfMin) != LongBacked.Min) return 13;
        if (l != LongBacked.Min) return 14;
        if (Interlocked.CompareExchange(ref l, LongBacked.Ninety, LongBacked.Min) != LongBacked.Min) return 15;
        if (l != LongBacked.Ninety) return 16;

        // Unsigned 32-bit backing, exercising a value whose signed reading is negative.
        UIntBacked u = UIntBacked.Max;
        if (Interlocked.CompareExchange(ref u, UIntBacked.FortyTwo, UIntBacked.Other) != UIntBacked.Max) return 17;
        if (u != UIntBacked.Max) return 18;
        if (Interlocked.CompareExchange(ref u, UIntBacked.FortyTwo, UIntBacked.Max) != UIntBacked.Max) return 19;
        if (u != UIntBacked.FortyTwo) return 20;

        // Instance field of a heap object, as CancellationTokenSource does it.
        Holder holder = new Holder();
        if (holder.TrySetState(States.NotifyingCompleteState, States.NotifyingState) != States.NotCanceledState) return 21;
        if (holder.State != States.NotCanceledState) return 22;
        if (holder.TrySetState(States.NotifyingState, States.NotCanceledState) != States.NotCanceledState) return 23;
        if (holder.State != States.NotifyingState) return 24;

        // Static field.
        if (Interlocked.CompareExchange(ref StaticState, States.NotifyingCompleteState, States.NotCanceledState) != States.NotifyingState) return 25;
        if (StaticState != States.NotifyingState) return 26;
        if (Interlocked.CompareExchange(ref StaticState, States.NotifyingCompleteState, States.NotifyingState) != States.NotifyingState) return 27;
        if (StaticState != States.NotifyingCompleteState) return 28;

        // Array element.
        States[] arr = new States[] { States.NotifyingState, States.NotCanceledState };
        if (Interlocked.CompareExchange(ref arr[1], States.NotifyingCompleteState, States.NotifyingState) != States.NotCanceledState) return 29;
        if (arr[1] != States.NotCanceledState) return 30;
        if (Interlocked.CompareExchange(ref arr[1], States.NotifyingCompleteState, States.NotCanceledState) != States.NotCanceledState) return 31;
        if (arr[1] != States.NotifyingCompleteState) return 32;
        if (arr[0] != States.NotifyingState) return 33;

        return 0;
    }
}
