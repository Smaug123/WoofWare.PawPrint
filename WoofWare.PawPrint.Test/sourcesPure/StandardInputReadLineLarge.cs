using System;

// Console.ReadLine over more than the 64 KiB a pipe holds on Linux and on
// Darwin: the 70 lines TestPureCases supplies for this case, so the launcher's
// write is still putting bytes in while the guest reads lines out, and one line
// straddles the point where the pipe first filled.
//
// Line i is 1000 copies of the letter 'a' + i % 26, and every line ends in
// "\n". Each line is checked at its ends and in its middle rather than whole,
// to keep the interpreted guest quick.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    const int Lines = 70;
    const int Width = 1000;

    static int Main(string[] args)
    {
        for (int i = 0; i < Lines; i++)
        {
            string? got = Console.ReadLine();
            if (got == null) return 1;
            if (got.Length != Width) return 2;
            char c = (char)('a' + i % 26);
            if (got[0] != c || got[Width / 2] != c || got[Width - 1] != c) return 3;
        }

        if (Console.ReadLine() != null) return 4;
        return 0;
    }
}
