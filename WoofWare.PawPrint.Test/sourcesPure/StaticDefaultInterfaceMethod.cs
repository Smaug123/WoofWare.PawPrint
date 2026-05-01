public interface IDefaultStaticScore<TSelf> where TSelf : IDefaultStaticScore<TSelf>
{
    static virtual int Score(TSelf value)
    {
        return 11;
    }
}

public struct DefaultStaticScore : IDefaultStaticScore<DefaultStaticScore>
{
}

public class Program
{
    private static int Score<T>(T value) where T : IDefaultStaticScore<T>
    {
        return T.Score(value);
    }

    public static int Main(string[] args)
    {
        return Score(default(DefaultStaticScore)) == 11 ? 0 : 1;
    }
}
