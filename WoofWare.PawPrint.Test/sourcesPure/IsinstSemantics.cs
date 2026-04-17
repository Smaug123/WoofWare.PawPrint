using System;

interface IMyInterface
{
}

interface IUnrelated
{
}

class Base
{
}

class Derived : Base, IMyInterface
{
}

class Unrelated
{
}

class Program
{
    static int Main(string[] args)
    {
        int result = 0;

        object derived = new Derived();

        // Exact type match: Derived is Derived
        if (derived is Derived)
            result += 1;

        // Base class match: Derived is Base
        if (derived is Base)
            result += 2;

        // Interface match: Derived is IMyInterface
        if (derived is IMyInterface)
            result += 4;

        // Non-match: Derived is not Unrelated
        if (derived is Unrelated)
            result += 100;

        // Non-match: Derived is not IUnrelated
        if (derived is IUnrelated)
            result += 100;

        // System.Object match: everything is object
        if (derived is object)
            result += 8;

        // null isinst always results in null (falsy)
        object nullRef = null;
        if (nullRef is Derived)
            result += 100;

        // 1 + 2 + 4 + 8 = 15
        return result;
    }
}
