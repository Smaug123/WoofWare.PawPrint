using System;

// `ldvirtftn` against a method declared in another assembly (System.Private.CoreLib). The
// sibling `LdvirtftnVirtualDispatch.cs` only ever names in-assembly declarations, so its
// tokens are MethodDefs; a call site typed as `object` names `System.Object::ToString`
// through a MemberReference instead, which is a different arm of the shared token
// resolution — and unlike the `ldftn` counterpart (`LdftnCrossAssembly.cs`, which takes a
// static `Math.Max`), this one combines that arm with real virtual dispatch back into
// *this* assembly's override.

public class LdvirtftnCrossAssemblyGreeter
{
    public override string ToString()
    {
        return "greeter";
    }
}

public class LdvirtftnCrossAssemblyPlain
{
}

class Program
{
    static int Main(string[] args)
    {
        // Receiver's runtime type overrides the corelib declaration: dispatch must land on
        // the override rather than on `Object::ToString`.
        object greeter = new LdvirtftnCrossAssemblyGreeter();
        Func<string> describeGreeter = greeter.ToString;
        if (describeGreeter() != "greeter")
        {
            return 1;
        }

        // Receiver's runtime type does not override it, so the corelib body is the answer.
        // `Object::ToString` returns the type's full name.
        object plain = new LdvirtftnCrossAssemblyPlain();
        Func<string> describePlain = plain.ToString;
        if (describePlain() != "LdvirtftnCrossAssemblyPlain")
        {
            return 2;
        }

        // A string receiver: the override lives in corelib too, so both the call-site
        // declaration and the resolved body are cross-assembly.
        object text = "hello";
        Func<string> describeText = text.ToString;
        if (describeText() != "hello")
        {
            return 3;
        }

        return 0;
    }
}
