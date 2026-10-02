using System;

// Console.In.ReadToEnd over a redirected standard input: the bytes TestPureCases
// supplies for this case, written into the guest's stdin pipe by PawPrint's
// launcher and by the oracle's alike, then the pipe closed. They are UTF-8,
// with characters of every encoded width, and no trailing newline; ReadToEnd
// returns all of them, and a second ReadToEnd returns "".
//
// The exit code is the index of the first check that failed; 0 means all passed.
class Program
{
    static int Main(string[] args)
    {
        string expected = "one\ntwo\r\nthree éè € \U0001F600\n\nend";

        string got = Console.In.ReadToEnd();
        if (got.Length != expected.Length) return 1;
        if (got != expected) return 2;
        if (Console.In.ReadToEnd() != "") return 3;
        if (Console.ReadLine() != null) return 4;

        return 0;
    }
}
