using System;

public class InterfaceDispatchTests
{
    public static int Main(string[] argv)
    {
        int result = 0;

        result |= TestBasicInterface();
        result |= TestExplicitImplementation() << 1;
        result |= TestMultipleInterfaces() << 2;
        result |= TestInterfaceInheritance() << 3;
        result |= TestDiamondInheritance() << 4;
        result |= TestGenericInterface() << 5;
        result |= TestCovariantInterface() << 6;
        result |= TestReimplementation() << 7;
        result |= TestStructInterface() << 8;
        result |= TestNullDispatch() << 9;

        return result;
    }

    // Test 1: Basic interface dispatch
    static int TestBasicInterface()
    {
        ISimple obj = new SimpleImpl();
        return obj.GetValue() == 42 ? 0 : 1;
    }

    // Test 2: Explicit interface implementation
    static int TestExplicitImplementation()
    {
        var obj = new ExplicitImpl();
        IExplicit iface = obj;

        // Direct call should return 10, interface call should return 20
        if (obj.GetValue() != 10) return 1;
        if (iface.GetValue() != 20) return 1;

        return 0;
    }

    // Test 3: Multiple interfaces
    static int TestMultipleInterfaces()
    {
        var obj = new MultiImpl();
        IFirst first = obj;
        ISecond second = obj;

        if (first.GetFirst() != 1) return 1;
        if (second.GetSecond() != 2) return 1;

        return 0;
    }

    // Test 4: Interface inheritance
    static int TestInterfaceInheritance()
    {
        IDerived obj = new DerivedImpl();
        IBase baseIface = obj;

        if (baseIface.GetBase() != 100) return 1;
        if (obj.GetDerived() != 200) return 1;

        return 0;
    }

    // Test 5: Diamond inheritance pattern
    static int TestDiamondInheritance()
    {
        var obj = new DiamondImpl();
        ILeft left = obj;
        IRight right = obj;
        IDiamond diamond = obj;

        if (left.GetValue() != 300) return 1;
        if (right.GetValue() != 300) return 1;
        if (diamond.GetDiamondValue() != 400) return 1;

        return 0;
    }

    // Test 6: Generic interface dispatch
    static int TestGenericInterface()
    {
        IGeneric<int> intObj = new GenericImpl<int>();
        IGeneric<string> strObj = new GenericImpl<string>();

        if (intObj.Process(5) != 10) return 1;
        if (strObj.Process("test") != 8) return 1;

        return 0;
    }

    // Test 7: Covariant interface dispatch
    static int TestCovariantInterface()
    {
        ICovariant<string> strCov = new CovariantImpl();
        ICovariant<object> objCov = strCov; // Covariance allows this

        object result = objCov.Get();
        if (!(result is string s && s == "covariant")) return 1;

        return 0;
    }

    // Test 8: Interface reimplementation in derived class
    static int TestReimplementation()
    {
        BaseClass baseObj = new DerivedClass();
        IReimpl iface = baseObj;

        // Should call derived implementation
        if (iface.Method() != 500) return 1;

        // Now test with base reference
        BaseClass pureBase = new BaseClass();
        IReimpl baseIface = pureBase;
        if (baseIface.Method() != 600) return 1;

        return 0;
    }

    // Test 9: Struct implementing interface
    static int TestStructInterface()
    {
        StructImpl s = new StructImpl { Value = 700 };
        ISimple boxed = s; // Boxing happens here

        if (boxed.GetValue() != 700) return 1;

        // Verify boxing created a copy
        s.Value = 800;
        if (boxed.GetValue() != 700) return 1; // Should still be 700

        return 0;
    }

    // Test 10: Null dispatch (should throw)
    static int TestNullDispatch()
    {
        ISimple nullRef = null;
        try
        {
            nullRef.GetValue();
            return 1; // Should have thrown
        }
        catch (NullReferenceException)
        {
            return 0; // Expected
        }
    }

    // Test interfaces and implementations

    interface ISimple
    {
        int GetValue();
    }

    class SimpleImpl : ISimple
    {
        public int GetValue() => 42;
    }

    interface IExplicit
    {
        int GetValue();
    }

    class ExplicitImpl : IExplicit
    {
        public int GetValue() => 10;
        int IExplicit.GetValue() => 20;
    }

    interface IFirst
    {
        int GetFirst();
    }

    interface ISecond
    {
        int GetSecond();
    }

    class MultiImpl : IFirst, ISecond
    {
        public int GetFirst() => 1;
        public int GetSecond() => 2;
    }

    interface IBase
    {
        int GetBase();
    }

    interface IDerived : IBase
    {
        int GetDerived();
    }

    class DerivedImpl : IDerived
    {
        public int GetBase() => 100;
        public int GetDerived() => 200;
    }

    interface ICommon
    {
        int GetValue();
    }

    interface ILeft : ICommon
    {
    }

    interface IRight : ICommon
    {
    }

    interface IDiamond : ILeft, IRight
    {
        int GetDiamondValue();
    }

    class DiamondImpl : IDiamond
    {
        public int GetValue() => 300;
        public int GetDiamondValue() => 400;
    }

    interface IGeneric<T>
    {
        int Process(T value);
    }

    class GenericImpl<T> : IGeneric<T>
    {
        public int Process(T value)
        {
            if (value is int i) return i * 2;
            if (value is string s) return s.Length * 2;
            return 0;
        }
    }

    interface ICovariant<out T>
    {
        T Get();
    }

    class CovariantImpl : ICovariant<string>
    {
        public string Get() => "covariant";
    }

    interface IReimpl
    {
        int Method();
    }

    class BaseClass : IReimpl
    {
        public virtual int Method() => 600;
    }

    class DerivedClass : BaseClass, IReimpl
    {
        public override int Method() => 500;
    }

    struct StructImpl : ISimple
    {
        public int Value;
        public int GetValue() => Value;
    }
}
