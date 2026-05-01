public interface IRootDiamondScore<TSelf> where TSelf : IRootDiamondScore<TSelf>
{
    static virtual int Score(TSelf value)
    {
        return 17;
    }
}

public interface ILeftDiamondScore<TSelf> : IRootDiamondScore<TSelf>
    where TSelf : ILeftDiamondScore<TSelf>
{
}

public interface IRightDiamondScore<TSelf> : IRootDiamondScore<TSelf>
    where TSelf : IRightDiamondScore<TSelf>
{
}

public struct DiamondStaticScore : ILeftDiamondScore<DiamondStaticScore>, IRightDiamondScore<DiamondStaticScore>
{
}

public class Program
{
    private static int Score<T>(T value) where T : IRootDiamondScore<T>
    {
        return T.Score(value);
    }

    public static int Main(string[] args)
    {
        return Score(default(DiamondStaticScore)) == 17 ? 0 : 1;
    }
}
