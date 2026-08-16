using System;
using System.Collections.Generic;

// A potential bug in MethodSpec(MemberReference) ldftn handling: if
// the spec's method-generic args reference the caller's class generic (e.g.
// `class C<A,B>` calling `Other<X>.Generic<B>`), the path passes spec.Signature
// raw to concretizeMethodForExecution alongside `extractedTypeArgs` (the
// target type's generics), which then re-substitutes the spec args against
// the wrong context. This test exercises that exact shape: a generic class
// captures a delegate over a BCL generic method on a generic target type,
// instantiated with its own class type parameter.

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
        // List<int>.ConvertAll<TOutput> is a generic method on a constructed
        // generic type (List<int>). Taking the address with B as the method
        // type argument produces MethodSpec(MemberReference) where:
        //   - extractedTypeArgs = [int]  (List<int>'s generics)
        //   - spec.Signature   = [B]     (referring to caller's class generic 1)
        // If concretizeMethodForExecution re-substitutes spec.Signature using
        // extractedTypeArgs as typeGenerics context, GenericTypeParameter 1
        // would index out of bounds in [int].
        var list = new List<int> { 1, 2, 3 };
        Func<Converter<int, B>, List<B>> f = list.ConvertAll<B>;
        var converted = f(_ => default!);
        return converted.Count == 3;
    }
}
