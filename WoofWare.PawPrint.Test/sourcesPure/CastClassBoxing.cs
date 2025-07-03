public class Program
{
    public struct Point
    {
        public int X;
        public int Y;

        public Point(int x, int y)
        {
            X = x;
            Y = y;
        }
    }

    public static int Main(string[] args)
    {
        Point p = new Point(10, 32);

        // Box the value type
        object boxed = p;

        // Cast boxed value type to object (should succeed)
        object obj = (object)boxed;

        // Unbox
        Point unboxed = (Point)obj;

        return unboxed.X + unboxed.Y;
    }
}
