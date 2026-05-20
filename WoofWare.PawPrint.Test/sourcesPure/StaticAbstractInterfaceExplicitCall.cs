public interface IStaticScore<TSelf> where TSelf : IStaticScore<TSelf>
{
    static abstract int Score(TSelf value);
}

public struct ExplicitStaticScore : IStaticScore<ExplicitStaticScore>
{
    static int IStaticScore<ExplicitStaticScore>.Score(ExplicitStaticScore value)
    {
        return 7;
    }
}

public class Program
{
    private static int Score<T>(T value) where T : IStaticScore<T>
    {
        return T.Score(value);
    }

    public static int Main(string[] args)
    {
        return Score(default(ExplicitStaticScore)) == 7 ? 0 : 1;
    }
}
