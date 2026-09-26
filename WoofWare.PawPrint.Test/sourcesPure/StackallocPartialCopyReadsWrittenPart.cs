using System;
using System.Runtime.CompilerServices;

// A `stackalloc` buffer of which only some bytes are written, copied whole to another buffer —
// once by `Span<T>.CopyTo`, a byte at a time underneath, and once by hand in eight-byte chunks,
// each chunk straddling written and unwritten bytes. Reading back only the bytes that were
// written must give what was written, on every runtime: a copy moves the unwritten bytes along
// without anything depending on them.

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* source = stackalloc byte[16];

        // Bytes 0-3 and 8-11 written; 4-7 and 12-15 never are.
        for (int i = 0; i < 4; i++)
        {
            source[i] = (byte)(i + 1);
            source[8 + i] = (byte)(10 * (i + 1));
        }

        byte* bySpan = stackalloc byte[16];
        new Span<byte>(source, 16).CopyTo(new Span<byte>(bySpan, 16));

        byte* byChunk = stackalloc byte[16];
        ((long*)byChunk)[0] = ((long*)source)[0];
        ((long*)byChunk)[1] = ((long*)source)[1];

        int sum = 0;

        for (int i = 0; i < 4; i++)
        {
            if (bySpan[i] != source[i]) return 1;
            if (bySpan[8 + i] != source[8 + i]) return 2;
            if (byChunk[i] != source[i]) return 3;
            if (byChunk[8 + i] != source[8 + i]) return 4;
            sum += bySpan[i] + byChunk[8 + i];
        }

        // An int read wholly within the written bytes of a chunk is defined too.
        if (*(int*)(byChunk + 8) != 0x281E140A) return 5;

        return sum == 1 + 2 + 3 + 4 + 10 + 20 + 30 + 40 ? 0 : 6;
    }
}
