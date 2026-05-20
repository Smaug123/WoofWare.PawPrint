public interface IInheritedStaticScore<TSelf> where TSelf : IInheritedStaticScore<TSelf>
{
    static virtual int Score(TSelf value)
    {
        return 1;
    }
}

public class BaseStaticScore<TSelf> : IInheritedStaticScore<TSelf>
    where TSelf : BaseStaticScore<TSelf>
{
    public static int Score(TSelf value)
    {
        return 5;
    }
}

public class DerivedStaticScoreFromBase : BaseStaticScore<DerivedStaticScoreFromBase>
{
}

public class Program
{
    private static int Score<T>(T value) where T : IInheritedStaticScore<T>
    {
        return T.Score(value);
    }

    public static int Main(string[] args)
    {
        return Score(new DerivedStaticScoreFromBase()) == 5 ? 0 : 1;
    }
}
