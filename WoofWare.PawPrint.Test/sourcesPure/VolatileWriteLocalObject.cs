using System.Threading;

class Program
{
    static int Main(string[] args)
    {
        object value = null;
        object replacement = new object();
        Volatile.Write(ref value, replacement);

        if (!object.ReferenceEquals(value, replacement)) return 1;

        Volatile.Write(ref value, null);
        if (value != null) return 2;

        return 0;
    }
}
