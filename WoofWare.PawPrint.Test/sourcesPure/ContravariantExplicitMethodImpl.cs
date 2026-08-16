// Regression for IlMachineStateExecution.findMatchingMethodImplBodies: when a class
// provides an explicit MethodImpl for IContravariant<object>.Set and the call site
// dispatches IContravariant<string>.Set (allowed by `in`-variance), the MethodImpl
// must still be selected. The trap: accepting the declaring-type match via variance
// but then rejecting the body in `signatureMatchesTarget` because parameter types are
// compared invariantly (`object` ≠ `string`).

using System;

interface IContravariant<in T> { void Set(T value); }

class ContravariantImpl : IContravariant<object>
{
    public object Value;
    void IContravariant<object>.Set(object value) { Value = value is string ? value : 42; }
}

class Program
{
    static int Main(string[] args)
    {
        // Hold the implementation under its concrete type only for inspection; the
        // virtual call goes through IContravariant<string>, whose Set(string) signature
        // differs from the MethodImpl body's Set(object) declaration, so a
        // candidate.ParameterTypes = methodToCall.ParameterTypes comparison rejects it.
        ContravariantImpl impl = new ContravariantImpl();
        IContravariant<string> strCon = impl;
        strCon.Set("from-contravariant");
        return (impl.Value is string s && s == "from-contravariant") ? 0 : 1;
    }
}
