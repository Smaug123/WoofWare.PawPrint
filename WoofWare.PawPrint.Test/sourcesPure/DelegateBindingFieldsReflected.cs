using System;
using System.Reflection;

// What `CreateDelegate` leaves in a delegate's private fields, read by reflection. On .NET 11 the
// `Delegate_BindToMethodInfo` QCall no longer writes the delegate: it reports the binding through a
// `BindToMethodDetails` struct, and CoreLib copies that into `_target`, `_methodPtr`,
// `_methodPtrAux` and `_extraData`. Measured on 10.0.12 and on 11.0.0-rc.1:
//
// * an open delegate's `_target` is the delegate itself, and its `_methodPtrAux` is not zero;
// * a closed delegate's `_target` is the object it closes over, and its `_methodPtrAux` is zero;
// * on .NET 11, `_extraData` holds the `MethodDesc*` of the method `CreateDelegate` was handed,
//   which `RuntimeMethodHandle.Value` answers too, even where the delegate is closed over an
//   object that overrides it and `_methodPtr` names the override. .NET 10 has no `_extraData`.
//
// Returns 0 on success, or the number of the first check that failed.

public class Base
{
    public virtual string Describe () => "base";
}

public class Derived : Base
{
    public override string Describe () => "derived";
}

public static class Program
{
    static object Field (Delegate d, string name)
    {
        FieldInfo field = typeof (Delegate).GetField (name, BindingFlags.Instance | BindingFlags.NonPublic);
        return field == null ? null : field.GetValue (d);
    }

    static string Twice (string s) => s + s;

    public static int Main ()
    {
        Derived receiver = new Derived ();
        MethodInfo declared = typeof (Base).GetMethod ("Describe");
        MethodInfo overriding = typeof (Derived).GetMethod ("Describe");

        Func<string> closed = (Func<string>) declared.CreateDelegate (typeof (Func<string>), receiver);

        // 1: the closed delegate dispatches to the override.
        if (closed () != "derived")
        {
            return 1;
        }

        // 2, 3: it holds the receiver, and nothing in `_methodPtrAux`.
        if (!ReferenceEquals (Field (closed, "_target"), receiver))
        {
            return 2;
        }

        if ((IntPtr) Field (closed, "_methodPtrAux") != IntPtr.Zero)
        {
            return 3;
        }

        Func<string, string> open = (Func<string, string>)
            typeof (Program).GetMethod ("Twice", BindingFlags.Static | BindingFlags.NonPublic)
                .CreateDelegate (typeof (Func<string, string>));

        // 4: the open delegate runs its target.
        if (open ("ab") != "abab")
        {
            return 4;
        }

        // 5, 6: it holds itself, and something in `_methodPtrAux`.
        if (!ReferenceEquals (Field (open, "_target"), open))
        {
            return 5;
        }

        if ((IntPtr) Field (open, "_methodPtrAux") == IntPtr.Zero)
        {
            return 6;
        }

        object extraData = Field (closed, "_extraData");

        if (extraData == null)
        {
            // .NET 10's layout, which records no `MethodDesc*` for a delegate it binds this way.
            return 0;
        }

        // 7, 8: `_extraData` names the method handed in, not the override `_methodPtr` names.
        if ((IntPtr) extraData != declared.MethodHandle.Value)
        {
            return 7;
        }

        if ((IntPtr) extraData == overriding.MethodHandle.Value)
        {
            return 8;
        }

        return 0;
    }
}
