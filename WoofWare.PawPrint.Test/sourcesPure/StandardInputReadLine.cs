using System;

// Console.ReadLine over a redirected standard input: the bytes TestPureCases
// supplies for this case, written into the guest's stdin pipe by PawPrint's
// launcher and by the oracle's alike, then the pipe closed.
//
// Lines end at "\n", "\r\n" or a lone "\r"; an empty line is "", not null; the
// last line needs no terminator; and once every byte has been read, ReadLine
// answers null, every time it is asked.
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    static int Main(string[] args)
    {
        string[] expected =
        {
            "first line",
            "second line, after a CRLF",
            "",
            "after an empty line",
            "after a lone CR",
            "café ☃ \U0001F600",
            "last, with no newline",
        };

        int check = 0;
        foreach (string line in expected)
        {
            check++;
            string? got = Console.ReadLine();
            if (got != line) return check;
        }

        check++;
        if (Console.ReadLine() != null) return check;
        check++;
        if (Console.ReadLine() != null) return check;
        check++;
        if (Console.In.Peek() != -1) return check;

        return 0;
    }
}
