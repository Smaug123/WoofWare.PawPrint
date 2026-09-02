using System;
using System.Buffers;

// `Environment.GetEnvironmentVariable` on CoreCLR runs
// Environment.Variables.Windows.cs on every platform: it calls the
// `kernel32!GetEnvironmentVariableW` QCall with a 128-char stack buffer, and
// while the answer exceeds the buffer it grows a `ValueStringBuilder` to that
// answer and calls again. On Unix that QCall is the PAL's, which holds the
// environment as UTF-8 and reports an insufficient buffer as the value's byte
// length plus one -- so a value of 100 two-byte characters, 100 code units,
// does not fit the 128-char buffer: the first call answers 201, the builder
// rents from `ArrayPool<char>.Shared`, and the second call fills the rented
// array. A shim that measured the required size in code units would answer 100
// on the first call and never touch the pool.
//
// The string the guest gets back is the same either way, since the retry loop
// converges; what differs is the pool. `ValueStringBuilder.ToString` returns
// the rented array without clearing it, so on the real runtime the next
// `Rent` of that size on this thread hands back an array still holding the
// value. That is the observation this file makes, and it is the only one a
// guest can make through the public API.
//
// Registered with the value under `environmentCases` in TestPureCases, which
// sets it in the oracle process's environment as well as in the kernel table.
public class TestEnvironmentVariableUtf8RequiredSize
{
    public static int Main(string[] argv)
    {
        string value = Environment.GetEnvironmentVariable("PAWPRINT_WIDE_VALUE");

        // Nothing may run between the call above and this Rent: string
        // interpolation, `Console`, and most formatting rent from the same
        // pool and would take the array first.
        char[] rented = ArrayPool<char>.Shared.Rent(201);

        if (value == null) return 1;
        if (value.Length != 100) return 2;

        for (int i = 0; i < value.Length; i++)
        {
            if (value[i] != 'é') return 3;
        }

        // 201 selects the 256-element bucket, the same one the builder's
        // `EnsureCapacity(201)` rented from and returned to.
        if (rented.Length != 256) return 4;

        for (int i = 0; i < 100; i++)
        {
            if (rented[i] != 'é') return 5;
        }

        // The terminator the QCall wrote after the value.
        if (rented[100] != '\0') return 6;

        return 0;
    }
}
