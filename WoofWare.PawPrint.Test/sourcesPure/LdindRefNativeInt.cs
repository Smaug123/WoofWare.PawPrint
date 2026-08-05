using System;

// Regression test for issue #711: `ldind.ref` through a managed pointer that
// has been widened to `native int` on the eval stack.
//
// ECMA-335 III.3.44 allows the address operand of `ldind.*` to be `native
// int` as well as `&`. Real CoreLib exploits this in
// `System.Threading.Tasks.Task.FromResult<TResult>` to reinterpret a
// concretely-typed cached task as `Task<TResult>` without paying for a real
// type check: it takes the address of a local of concrete type `Task<bool>`,
// widens that address to `native int` via an explicit unsafe pointer cast
// (`Task<TResult>*`), then reads back through it. Because the cast target is
// a *closed generic reference type* (`Task<TResult>` is a class for every
// TResult) rather than a bare type parameter, the compiler knows the
// dereference is reference-shaped and so emits `ldind.ref`, not `ldobj`.
//
// `Reinterpret<T>` below reproduces that exact IL shape
// (`ldloca.s; conv.u; ldind.ref`) without going anywhere near Task, using an
// ordinary generic reference-type wrapper instead of Task<T>. Verified with
// IlDump against the compiled test assembly to actually emit
// `ldloca.s 0 / conv.u / ldind.ref`, not `ldobj`.
unsafe class LdindRefNativeInt
{
    sealed class Box<T>
    {
        public T Value;

        public Box (T value)
        {
            Value = value;
        }
    }

    static Box<T> Reinterpret<T> (Box<bool> source)
    {
        Box<bool> local = source;

        // `&local` is `Box<bool>&`; the cast to `Box<T>*` forces a `conv.u`
        // before the address is dereferenced, so by the time `ldind.ref`
        // executes, the address is native-int-typed on the eval stack, not
        // `&`-typed.
        return *(Box<T>*) &local;
    }

    static int Main (string[] args)
    {
        var box = new Box<bool> (true);

        Box<bool> result = Reinterpret<bool> (box);

        if (!ReferenceEquals (box, result))
        {
            return 1;
        }

        if (result.Value != true)
        {
            return 2;
        }

        return 0;
    }
}
