// Regression for IlMachineStateExecution.methodReferenceMatchesTarget: when a
// contravariant interface has overloaded methods, each MethodImpl must bind to
// its specific virtual slot. An earlier attempt at the variance fix relaxed the
// parameter check to allow contravariant assignability, which made *both*
// `Consume(object)` and `Consume(IComparable)` match a dispatch to
// `Consume(string)` (string ≤ object and string ≤ IComparable), tripping the
// "multiple MethodImpl bodies" guard. The correct rule keys the match on the
// underlying MethodDefinitionHandle of the interface slot.

using System;

interface IConsumer<in T>
{
    void Consume(T value);
    void Consume(IComparable other);
}

class TwoBodies : IConsumer<object>
{
    public string Tag;
    void IConsumer<object>.Consume(object value) { Tag = "object-overload:" + (value as string ?? "?"); }
    void IConsumer<object>.Consume(IComparable other) { Tag = "comparable-overload"; }
}

class Program
{
    static int Main(string[] args)
    {
        TwoBodies impl = new TwoBodies();
        IConsumer<string> strCon = impl;
        // Dispatches IConsumer<string>::Consume(T=string), i.e. Consume(string).
        // string also implements IComparable, so a relaxed parameter check would
        // also accept the Consume(IComparable) MethodImpl as a candidate.
        strCon.Consume("hello");
        return impl.Tag == "object-overload:hello" ? 0 : 1;
    }
}
