public interface IBaseInheritedDefaultScore<TSelf> where TSelf : IBaseInheritedDefaultScore<TSelf>
{
    static virtual int Score(TSelf value)
    {
        return 1;
    }
}

public interface IDerivedInheritedDefaultScore<TSelf> : IBaseInheritedDefaultScore<TSelf>
    where TSelf : IDerivedInheritedDefaultScore<TSelf>
{
    static int IBaseInheritedDefaultScore<TSelf>.Score(TSelf value)
    {
        return 2;
    }
}

public class BaseInheritedDefaultScore<TSelf> : IDerivedInheritedDefaultScore<TSelf>
    where TSelf : BaseInheritedDefaultScore<TSelf>
{
}

public class DerivedInheritedDefaultScore : BaseInheritedDefaultScore<DerivedInheritedDefaultScore>
{
}

public class Program
{
    private static int Score<T>(T value) where T : IBaseInheritedDefaultScore<T>
    {
        return T.Score(value);
    }

    public static int Main(string[] args)
    {
        return Score(new DerivedInheritedDefaultScore()) == 2 ? 0 : 1;
    }
}
