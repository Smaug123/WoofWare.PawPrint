public class Program
{
    public class Container<T>
    {
        public T Value { get; set; }
    }

    public static int Main(string[] args)
    {
        Container<int> intContainer = new Container<int> { Value = 42 };

        // Cast generic type to object
        object obj = (object)intContainer;

        // Check type and cast back
        if (obj is Container<int> container)
        {
            return container.Value;
        }

        return 0;
    }
}
