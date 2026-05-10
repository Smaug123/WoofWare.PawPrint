using System.Threading;

class Holder
{
    public object Field;
}

class Program
{
    static int Main(string[] args)
    {
        Holder h = new Holder();
        object initial = new object();
        h.Field = initial;

        object replacement = new object();
        Volatile.Write(ref h.Field, replacement);

        if (!ReferenceEquals(h.Field, replacement)) return 1;
        if (ReferenceEquals(h.Field, initial)) return 2;

        Volatile.Write(ref h.Field, null);
        if (h.Field != null) return 3;

        return 0;
    }
}
