using System;

// Exercises castclass (explicit cast) failure paths — each must throw InvalidCastException.
// Every test returns 0 on success, nonzero on failure.

interface IChild
{
}

class Base
{
}

class Middle : Base, IChild
{
}

class Leaf : Middle
{
}

class Unrelated
{
}

class Program
{
    static int TestFailUnrelatedType()
    {
        // Cast Base to Unrelated — completely unrelated types.
        object o = new Base();
        try
        {
            Unrelated u = (Unrelated)o;
            return 1; // should not reach
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailDowncastActualBase()
    {
        // Runtime type is Base, trying to downcast to Middle — must throw.
        object o = new Base();
        try
        {
            Middle m = (Middle)o;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailDowncastMiddleToLeaf()
    {
        // Runtime type is Middle, trying to downcast to Leaf — must throw.
        object o = new Middle();
        try
        {
            Leaf l = (Leaf)o;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailCastToUnimplementedInterface()
    {
        // Base does not implement IChild.
        object o = new Base();
        try
        {
            IChild i = (IChild)o;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailCastUnrelatedToInterface()
    {
        // Unrelated does not implement IChild.
        object o = new Unrelated();
        try
        {
            IChild i = (IChild)o;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailCastBetweenSiblings()
    {
        // Middle and Unrelated are siblings (both derive from object), not related.
        object o = new Middle();
        try
        {
            Unrelated u = (Unrelated)o;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailCastFromInterfaceToWrongClass()
    {
        // IChild-typed ref holding a Middle — cast to Unrelated must throw.
        IChild i = new Middle();
        try
        {
            Unrelated u = (Unrelated)(object)i;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestFailCastFromInterfaceToWrongDerived()
    {
        // IChild-typed ref holding a Middle — cast to Leaf must throw (Middle is not Leaf).
        IChild i = new Middle();
        try
        {
            Leaf l = (Leaf)(object)i;
            return 1;
        }
        catch (InvalidCastException)
        {
            return 0;
        }
    }

    static int TestInvalidCastIsCatchableAsException()
    {
        // InvalidCastException derives from SystemException derives from Exception.
        // Catch as Exception to verify the type hierarchy works.
        object o = new Base();
        try
        {
            Middle m = (Middle)o;
            return 1;
        }
        catch (Exception)
        {
            return 0;
        }
    }

    static int Main(string[] args)
    {
        int result = 0;
        result += TestFailUnrelatedType();
        result += TestFailDowncastActualBase();
        result += TestFailDowncastMiddleToLeaf();
        result += TestFailCastToUnimplementedInterface();
        result += TestFailCastUnrelatedToInterface();
        result += TestFailCastBetweenSiblings();
        result += TestFailCastFromInterfaceToWrongClass();
        result += TestFailCastFromInterfaceToWrongDerived();
        result += TestInvalidCastIsCatchableAsException();
        return result;
    }
}
