class Program
{
    struct Point { public int X; public int Y; }

    static T MakeDefault<T>() where T : struct
    {
        T value = default;
        return value;
    }

    static int Main(string[] args)
    {
        Point p = MakeDefault<Point>();
        return (p.X == 0 && p.Y == 0) ? 0 : 1;
    }
}
