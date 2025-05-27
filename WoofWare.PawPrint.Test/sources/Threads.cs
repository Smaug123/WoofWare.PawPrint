using System.Threading.Tasks;

namespace HelloWorldApp
{
    class Program
    {
        static async System.Threading.Tasks.Task<int> Main(string[] args)
        {
            var result = await Task.Run(() => 3);
            return result;
        }
    }
}
