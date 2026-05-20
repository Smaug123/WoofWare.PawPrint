public interface IBaseStaticScore<TSelf> where TSelf : IBaseStaticScore<TSelf>
{
    static virtual int Score(TSelf value)
    {
        return 1;
    }
}

public interface IDerivedStaticScore<TSelf> : IBaseStaticScore<TSelf>
    where TSelf : IDerivedStaticScore<TSelf>
{
    static int IBaseStaticScore<TSelf>.Score(TSelf value)
    {
        return 2;
    }
}

public struct DerivedStaticScore : IDerivedStaticScore<DerivedStaticScore>
{
}

public class Program
{
    private static int Score<T>(T value) where T : IBaseStaticScore<T>
    {
        return T.Score(value);
    }

    public static int Main(string[] args)
    {
        return Score(default(DerivedStaticScore)) == 2 ? 0 : 1;
    }
}
