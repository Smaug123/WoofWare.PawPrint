using System;

// A delegate over an instance method whose receiver is null. CoreCLR refuses to build one:
// `MulticastDelegate.CtorClosed` throws `ArgumentException(Arg_DlgtNullInst)`
// (MulticastDelegate.CoreCLR.cs:552-556), and there is no `newobj` route past it — an *open*
// instance delegate is made only by `Delegate.CreateDelegate`, which records the target in
// `_methodPtrAux` instead.
//
// Both shapes here are asked for: a non-generic declaring type, and a generic one. They matter
// separately because `Delegate.GetMethodImpl` dereferences `_target` only when the declaring type
// is generic (Delegate.CoreCLR.cs:189), so only the second would fault if such a delegate were
// allowed to exist.
//
// Returns 0 on success, or a two-digit code `a * 10 + b` naming what each shape did instead:
// 1 = a null delegate came back, 2 = it constructed, 4 = NullReferenceException, 5 = some other
// exception. Correct behaviour is 3 (ArgumentException) for both, so 0.

public class Gen<T>
{
    public int M ()
    {
        return 1;
    }
}

public class NonGen
{
    public int M ()
    {
        return 1;
    }
}

public class DelegateOverNullInstanceReceiver
{
    // Opaque to the compiler, so the delegate creation is not folded away.
    private static NonGen MakeNull ()
    {
        return null;
    }

    private static Gen<string> MakeNullGen ()
    {
        return null;
    }

    public static int Main (string[] argv)
    {
        int a;

        try
        {
            Func<int> f = MakeNull ().M;
            a = f == null ? 1 : 2;
        }
        catch (ArgumentException)
        {
            a = 3;
        }
        catch (NullReferenceException)
        {
            a = 4;
        }
        catch (Exception)
        {
            a = 5;
        }

        int b;

        try
        {
            Func<int> g = MakeNullGen ().M;
            b = g.Method == null ? 1 : 2;
        }
        catch (ArgumentException)
        {
            b = 3;
        }
        catch (NullReferenceException)
        {
            b = 4;
        }
        catch (Exception)
        {
            b = 5;
        }

        if (a == 3 && b == 3) return 0;

        return a * 10 + b;
    }
}
