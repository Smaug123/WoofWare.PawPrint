public class TestStindFloat
{
    public static unsafe int Main(string[] argv)
    {
        float f = 1.0f;
        float* fPtr = &f;
        *fPtr = 2.5f;

        if (f != 2.5f)
        {
            return 1;
        }

        double d = 1.0;
        double* dPtr = &d;
        *dPtr = -3.25;

        if (d != -3.25)
        {
            return 2;
        }

        return 0;
    }
}
