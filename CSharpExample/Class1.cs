using System;
using System.Collections.Generic;
using System.Linq;

namespace HelloWorldApp
{
    interface IGeneric<T>
    {
        int Process(T value);
    }

    class GenericImpl<T> : IGeneric<T>
    {
        public int Process(T value)
        {
            if (typeof(T) == typeof(int))
            {
                return 3;
            }

            if (typeof(T) == typeof(string))
            {
                return 5;
            }
            return 0;
        }
    }

    class Program
    {
        static int Main(string[] args)
        {
            Console.WriteLine("Hello");
            return 0;
        }
    }
}
