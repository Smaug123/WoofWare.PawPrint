namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int[] array = new[] { 1, 2, 3 };

            int sum = 0;
            for (int i = 0; i < array.Length; i++)
            {
                sum += array[i];
            }

            if (sum != 6)
            {
                return 1;
            }

            if (array[0] != 1)
            {
                return 2;
            }

            if (array[1] != 2)
            {
                return 3;
            }

            if (array[2] != 3)
            {
                return 4;
            }

            return 0;
        }
    }
}
