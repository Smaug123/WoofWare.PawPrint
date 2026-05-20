using System;

class OpenDefinition<T> { }

class Program
{
    static int Main(string[] args)
    {
        if (!typeof(OpenDefinition<>).IsGenericTypeDefinition)
            return 1;

        if (typeof(OpenDefinition<int>).IsGenericTypeDefinition)
            return 2;

        if (typeof(int).IsGenericTypeDefinition)
            return 3;

        if (typeof(OpenDefinition<>).IsConstructedGenericType)
            return 4;

        if (!typeof(OpenDefinition<int>).IsConstructedGenericType)
            return 5;

        if (typeof(int).IsConstructedGenericType)
            return 6;

        return 0;
    }
}
