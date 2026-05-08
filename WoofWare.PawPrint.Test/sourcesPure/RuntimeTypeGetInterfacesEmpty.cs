using System;

namespace RuntimeTypeGetInterfacesEmpty
{
    class Program
    {
        static int Main(string[] args)
        {
            return typeof(Program).GetInterfaces().Length;
        }
    }
}
