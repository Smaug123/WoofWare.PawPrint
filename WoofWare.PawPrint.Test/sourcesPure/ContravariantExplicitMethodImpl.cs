// Regression for IlMachineStateExecution.findMatchingMethodImplBodies: when a class
// provides an explicit MethodImpl for IContravariant<object>.Set and the call site
// dispatches IContravariant<string>.Set (allowed by `in`-variance), the MethodImpl
// must still be selected. Earlier we accepted the declaring-type match via variance
// but then rejected the body in `signatureMatchesTarget` because parameter types were
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
        // differs from the MethodImpl body's Set(object) declaration. Earlier we accepted
        // the declaring-type match via `in`-variance but then rejected the body because
        // candidate.ParameterTypes = methodToCall.ParameterTypes compared object vs string.
        ContravariantImpl impl = new ContravariantImpl();
        IContravariant<string> strCon = impl;
        strCon.Set("from-contravariant");
        return (impl.Value is string s && s == "from-contravariant") ? 0 : 1;
    }
}
