using System;
using System.Collections.Generic;

// Direct-call counterpart to LdftnCrossAssemblyMethodSpecCallerGeneric.cs.
// Exercises the same MethodSpec(MemberReference) shape (caller's class generic
// used as a method-generic argument on a constructed BCL generic type) but via
// `call` rather than `ldftn`, to catch the equivalent re-substitution bug in
// `UnaryMetadataCallOps.executeCall`.

class Program
{
    static int Main(string[] args)
    {
        return new C<int, string>().Run() ? 0 : 1;
    }
}

class C<A, B>
{
    public bool Run()
    {
        var list = new List<int> { 1, 2, 3 };
        // List<int>::ConvertAll<B> — MethodSpec(MemberReference), target type
        // generics [int], method generics [B] (caller's class generic 1).
        var converted = list.ConvertAll<B>(_ => default!);
        return converted.Count == 3;
    }
}
