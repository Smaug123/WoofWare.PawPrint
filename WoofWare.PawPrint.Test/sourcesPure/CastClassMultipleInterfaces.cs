public class Program
{
    public interface IReadable
    {
        string Read();
    }

    public interface IWritable
    {
        void Write(string data);
    }

    public class File : IReadable, IWritable
    {
        private string content = "Hello";

        public string Read()
        {
            return content;
        }

        public void Write(string data)
        {
            content = data;
        }
    }

    public static int Main(string[] args)
    {
        File file = new File();

        // Cast to first interface
        IReadable readable = (IReadable)file;

        // Cast to second interface
        IWritable writable = (IWritable)file;

        return readable != null && writable != null ? 42 : 0;
    }
}
