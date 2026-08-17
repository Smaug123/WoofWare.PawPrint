using System;
using System.Reflection;

// FieldInfo.SetValue, whose one primitive is the RuntimeFieldHandle_SetValue QCall. Every shape
// here is written through reflection and read back with an *ordinary* field access: PawPrint does
// not implement RuntimeFieldHandle_GetValue, so witnessing a write with FieldInfo.GetValue would
// fail for an unrelated reason.

enum Colour
{
    None = 0,
    Red = 1,
    Green = 2,
    Blue = 3,
}

struct Point
{
    public int X;
    public int Y;
}

class Target
{
    public int Number;
    public string Text;
    public object Anything;
    public Colour Shade;
}

// A precise-init type (an explicit static constructor suppresses beforefieldinit), so the
// initialiser is guaranteed not to have run before the reflective set below. The initialiser
// overwrites Total, which is what makes the ordering observable: if SetValue ran it *first*, the
// 42 survives; if the later read of Total ran it instead, the 7 clobbers the 42.
static class LazyHolder
{
    public static int Total;

    static LazyHolder()
    {
        Total = 7;
    }
}

class ThreadStaticHolder
{
    [ThreadStatic]
    public static int PerThread;
}

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static int Main()
    {
        Target target = new Target();

        // Instance field of primitive type: the box's payload is read out and stored.
        typeof(Target).GetField("Number").SetValue(target, 42);
        Check(target.Number == 42);

        // Instance field of reference type: the object reference is copied straight through.
        typeof(Target).GetField("Text").SetValue(target, "hello");
        Check(target.Text == "hello");

        // A null for a reference-typed field really does arrive as null; CheckValue leaves it
        // alone rather than boxing a default.
        target.Anything = "not null yet";
        typeof(Target).GetField("Anything").SetValue(target, null);
        Check(target.Anything == null);

        // Storing a derived value into an object-typed field.
        typeof(Target).GetField("Anything").SetValue(target, "later");
        Check((string)target.Anything == "later");

        // An enum-typed field written from a *boxed underlying* value. CheckValue converts only
        // when the CorElementTypes differ, so this box stays typed System.Int32 all the way into
        // the QCall, and the store depends on the enum/underlying unboxing relaxation.
        typeof(Target).GetField("Shade").SetValue(target, 2);
        Check(target.Shade == Colour.Green);

        // The same field written from a properly-typed enum box.
        typeof(Target).GetField("Shade").SetValue(target, Colour.Blue);
        Check(target.Shade == Colour.Blue);

        // A boxed struct as the target: the write lands in the box, and is visible when the box
        // is unboxed again. (The original value type is unaffected — that is the point of a box.)
        Point original = new Point ();
        original.X = 1;
        original.Y = 2;
        object boxed = original;
        typeof(Point).GetField("X").SetValue(boxed, 5);
        Check(((Point)boxed).X == 5);
        Check(((Point)boxed).Y == 2);
        Check(original.X == 1);

        // A [ThreadStatic] field. This is the one field kind for which CoreCLR itself reports
        // IsFastPathSupported = false, so both runtimes reach the QCall by the same managed route.
        typeof(ThreadStaticHolder).GetField("PerThread").SetValue(null, 11);
        Check(ThreadStaticHolder.PerThread == 11);

        // A static field on a type whose initialiser has not yet run, which forces the QCall to
        // run it. Reading Total afterwards cannot run it a second time, so 42 surviving is
        // evidence the initialiser ran *before* the store rather than after it.
        typeof(LazyHolder).GetField("Total").SetValue(null, 42);
        Check(LazyHolder.Total == 42);

        // Repeat sets go through a differently-configured managed accessor (the first call
        // reports the class as initialised, which switches FieldAccessor off its first-call
        // state), so exercise the second one too.
        typeof(Target).GetField("Number").SetValue(target, -3);
        Check(target.Number == -3);

        return firstFailure;
    }
}
