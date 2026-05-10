namespace StringEmpty
{
    class Program
    {
        static int Main(string[] args)
        {
            string e = string.Empty;
            if (e is null) return 1;
            if (e.Length != 0) return 2;
            if (!ReferenceEquals(e, "")) return 3;
            return 0;
        }
    }
}
