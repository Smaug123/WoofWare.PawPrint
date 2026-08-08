using System.Threading.Tasks;

public static class TaskDelayWait
{
    public static int Main(string[] args)
    {
        Task.Delay(1).Wait();
        return 0;
    }
}
