public class SwitchInstruction
{
    static int Classify(int value)
    {
        switch (value)
        {
            case 0:
                return 10;
            case 1:
                return 11;
            case 2:
                return 12;
            case 3:
                return 13;
            default:
                return 99;
        }
    }

    public static int Main(string[] args)
    {
        if (Classify(-1) != 99) return 1;
        if (Classify(0) != 10) return 2;
        if (Classify(1) != 11) return 3;
        if (Classify(2) != 12) return 4;
        if (Classify(3) != 13) return 5;
        if (Classify(4) != 99) return 6;

        return 0;
    }
}
