namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            var a = new int[3, 4];
            if ((object)a == null)
            {
                return 1;
            }

            var b = new int[3, 4];
            if ((object)b == null)
            {
                return 2;
            }

            if (object.ReferenceEquals(a, b))
            {
                return 3;
            }

            return 0;
        }
    }
}
