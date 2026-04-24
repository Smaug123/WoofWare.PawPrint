public class VirtualDispatchBodiedOverride
{
    public static int Main(string[] argv)
    {
        Base receiver = new Derived();

        return receiver.Value() == 2 ? 0 : 1;
    }

    class Base
    {
        public virtual int Value() => 1;
    }

    class Derived : Base
    {
        public override int Value() => 2;
    }
}
