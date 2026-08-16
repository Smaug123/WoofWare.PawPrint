using System;
using System.IO;
using System.Text;

// Reading seeded files through the BCL, which on Unix is `pread(2)` and
// nothing else: `RandomAccess.ReadAtOffset` passes an explicit offset on every
// call, and the sequential position a `FileStream` appears to have lives in
// *managed* state (`OSFileStreamStrategy._filePosition`), not in the kernel.
//
// The two routes below issue materially different syscall sequences, which is
// why both are here:
//
//   * `File.ReadAllBytes` stats the file and then issues **one** `pread` at
//     offset 0 for the whole length. It therefore never exercises a non-zero
//     offset, and an implementation that ignored `fileOffset` entirely would
//     pass every check that used only this route.
//   * `File.ReadAllText`/`ReadAllLines`/a raw `FileStream` go through
//     `StreamReader`, which reads in 4096-byte chunks at increasing offsets. So
//     the >4 KB file below is what actually pins the offset, and it has to be
//     read by one of those rather than by `ReadAllBytes`.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    static int Main(string[] args)
    {
        int check;

        // The simple case.
        check = 1;
        byte[] b = File.ReadAllBytes("f");
        if (b.Length != 5) return check;
        check = 2;
        if (Encoding.UTF8.GetString(b) != "hello") return check;

        // An empty file: `pread` returns 0, which must read as "end of file"
        // rather than as an error.
        check = 3;
        if (File.ReadAllBytes("empty").Length != 0) return check;
        check = 4;
        if (File.ReadAllText("empty") != "") return check;

        // Bytes, not characters. The seed holds three bytes (C3 9F 78) encoding
        // two characters, so a handler counting .NET chars rather than bytes
        // disagrees here and nowhere else in this file.
        check = 5;
        byte[] mb = File.ReadAllBytes("mb");
        if (mb.Length != 3) return check;
        check = 6;
        if (mb[0] != 0xC3 || mb[1] != 0x9F || mb[2] != 0x78) return check;
        check = 7;
        if (File.ReadAllText("mb") != "ßx") return check;

        // The >4 KB file, through the chunked reader. `big` is 10000 bytes of a
        // repeating 251-byte cycle: 251 is coprime to 4096, so every chunk
        // boundary lands at a different phase of the pattern and an off-by-one
        // in the offset or the transfer count shifts bytes visibly rather than
        // landing on an identical byte.
        check = 8;
        string big = File.ReadAllText("big");
        if (big.Length != 10000) return check;
        check = 9;
        for (int i = 0; i < 10000; i++)
        {
            if (big[i] != (char)('a' + (i % 251) % 26)) return check;
        }

        // ...and the same file through the single-`pread` route, so the two
        // agree. An implementation that mishandled only the chunked path would
        // fail check 9 while passing this.
        check = 10;
        byte[] bigBytes = File.ReadAllBytes("big");
        if (bigBytes.Length != 10000) return check;
        check = 11;
        for (int i = 0; i < 10000; i++)
        {
            if (bigBytes[i] != (byte)('a' + (i % 251) % 26)) return check;
        }

        // Explicit non-zero offsets, which is what `RandomAccess` is for. Seek
        // is a managed field update, so this is one `pread` at offset 2.
        check = 12;
        using (var fs = File.OpenRead("f"))
        {
            fs.Seek(2, SeekOrigin.Begin);
            byte[] tail = new byte[3];
            if (fs.Read(tail, 0, 3) != 3) return check;
            check = 13;
            if (Encoding.UTF8.GetString(tail) != "llo") return check;

            // Reading at the very end yields 0, the BCL's end-of-stream signal.
            check = 14;
            if (fs.Read(tail, 0, 3) != 0) return check;
        }

        // A read whose requested length runs past the end is short, not an
        // error: 5-byte file, 64-byte request.
        check = 15;
        using (var fs = File.OpenRead("f"))
        {
            byte[] big2 = new byte[64];
            if (fs.Read(big2, 0, 64) != 5) return check;
        }

        // Line splitting, a third route through the same reader.
        check = 16;
        string[] lines = File.ReadAllLines("lines");
        if (lines.Length != 3) return check;
        check = 17;
        if (lines[0] != "one" || lines[1] != "two" || lines[2] != "three") return check;

        // Reading a file twice gives the same answer: `pread` does not consume,
        // and PawPrint's descriptor carries no offset that a first read could
        // have advanced.
        check = 18;
        if (Encoding.UTF8.GetString(File.ReadAllBytes("f")) != "hello") return check;

        // Two handles open at once, reading independently. Both take
        // `FileShare.Read` so the locks are compatible; the point is that each
        // has its own managed position, so interleaving them must not make them
        // share one.
        check = 19;
        using (var a = File.OpenRead("f"))
        using (var c = File.OpenRead("f"))
        {
            if (a.ReadByte() != 'h') return check;
            check = 20;
            if (c.ReadByte() != 'h') return check;
            check = 21;
            if (a.ReadByte() != 'e') return check;
        }

        return 0;
    }
}
