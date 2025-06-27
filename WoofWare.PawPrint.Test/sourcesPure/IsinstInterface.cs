public class Program
{
    public interface IDisposable
    {
        void Dispose();
    }

    public class Resource : IDisposable
    {
        public int Id { get; set; }

        public void Dispose()
        {
            // Cleanup
        }
    }

    public static int Main(string[] args)
    {
        Resource resource = new Resource { Id = 42 };

        // 'as' operator uses isinst instruction
        IDisposable disposable = resource as IDisposable;

        return disposable != null ? resource.Id : 0;
    }
}
