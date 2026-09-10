using System;
using System.Reflection;

// FieldInfo.GetValue, whose one primitive under PawPrint is the RuntimeFieldHandle_GetValue
// QCall: `IsFastPathSupported` answers false, so the managed accessor never takes its
// address-based paths and every reflective field read lands in that QCall. Every value here is
// written with an *ordinary* field access and read back through reflection, the mirror image of
// ReflectionFieldSetValue.cs.

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

struct Pair
{
    public string Label;
    public int Count;
}

class Target
{
    public int Number = 42;
    public long Big = 1L << 40;
    public double Ratio = 1.5;
    public bool Flag = true;
    public char Letter = 'q';
    public string Text = "hello";
    public object Anything;
    public Colour Shade = Colour.Green;
    public Point Where;
    public Pair Labelled;
    public int? Maybe;
    public IntPtr Handle = (IntPtr)77;
    private int secret = 9;

    public int RevealSecret() => secret;
}

// No static field here: enumerating a closed generic type's fields calls
// `RuntimeFieldHandle.GetStaticFieldForGenericType` for each static one, a second primitive that
// ReflectionFieldGetValueGenericStatic.cs covers.
class Gen<T>
{
    public T Value;
}

// A precise-init type (an explicit static constructor suppresses beforefieldinit), so the
// initialiser is guaranteed not to have run before the reflective read below. The read must run
// it *first*: a runtime that read the slot and only then ran the initialiser would answer 0.
static class LazyHolder
{
    public static int Total;

    static LazyHolder()
    {
        Total = 7;
    }
}

// No initialiser at all, and nothing ever writes these, so the reflective read is the first
// touch of the storage and must answer the field type's default.
static class Untouched
{
    public static int Never;
    public static string NeverSet;
}

class ThreadStaticHolder
{
    [ThreadStatic]
    public static int PerThread;
}

static class ReadOnlyHolder
{
    public static readonly string Name = "fixed";
    public static readonly Point Origin = new Point { X = 3, Y = 4 };
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

    static FieldInfo Field(Type type, string name)
    {
        return type.GetField(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
    }

    static int Main()
    {
        Target target = new Target();

        // Primitive-typed instance fields come back boxed as the field's own type.
        object number = Field(typeof(Target), "Number").GetValue(target);
        Check(number.GetType() == typeof(int));
        Check((int)number == 42);
        Check((long)Field(typeof(Target), "Big").GetValue(target) == (1L << 40));
        Check((double)Field(typeof(Target), "Ratio").GetValue(target) == 1.5);
        Check((bool)Field(typeof(Target), "Flag").GetValue(target) == true);
        Check((char)Field(typeof(Target), "Letter").GetValue(target) == 'q');

        // A reference-typed field hands back the very reference it holds, not a copy.
        Check(ReferenceEquals(Field(typeof(Target), "Text").GetValue(target), target.Text));

        // A null reference really does come back as null.
        Check(Field(typeof(Target), "Anything").GetValue(target) == null);

        // An object-typed field holding a box hands back that box, not a re-boxed copy.
        target.Anything = 12345;
        Check(ReferenceEquals(Field(typeof(Target), "Anything").GetValue(target), target.Anything));

        // An enum-typed field boxes as the enum, not as its underlying integer.
        object shade = Field(typeof(Target), "Shade").GetValue(target);
        Check(shade.GetType() == typeof(Colour));
        Check((Colour)shade == Colour.Green);

        // A struct-typed field comes back as a *copy*: the box is unaffected by a later write to
        // the field, and a fresh read sees the write.
        target.Where = new Point { X = 1, Y = 2 };
        object where = Field(typeof(Target), "Where").GetValue(target);
        Check(where.GetType() == typeof(Point));
        Check(((Point)where).X == 1 && ((Point)where).Y == 2);
        target.Where.X = 100;
        Check(((Point)where).X == 1);
        Check(((Point)Field(typeof(Target), "Where").GetValue(target)).X == 100);

        // A struct holding a reference boxes the same way.
        target.Labelled = new Pair { Label = "tag", Count = 6 };
        object labelled = Field(typeof(Target), "Labelled").GetValue(target);
        Check(((Pair)labelled).Label == "tag" && ((Pair)labelled).Count == 6);

        // A Nullable<T> field follows the boxing rule for Nullable<T>: null when it has no
        // value, and a box of the underlying T when it does — never a box of Nullable<T> itself.
        Check(Field(typeof(Target), "Maybe").GetValue(target) == null);
        target.Maybe = 5;
        object maybe = Field(typeof(Target), "Maybe").GetValue(target);
        Check(maybe.GetType() == typeof(int));
        Check((int)maybe == 5);

        // IntPtr is a value type with no fields of its own in the ordinary sense; it still boxes
        // as itself.
        object handle = Field(typeof(Target), "Handle").GetValue(target);
        Check(handle.GetType() == typeof(IntPtr));
        Check((IntPtr)handle == (IntPtr)77);

        // A private field is readable through reflection.
        Check((int)Field(typeof(Target), "secret").GetValue(target) == target.RevealSecret());

        // A boxed struct as the target: the field is read out of the box.
        object boxedPoint = new Point { X = 5, Y = 6 };
        Check((int)Field(typeof(Point), "X").GetValue(boxedPoint) == 5);
        Check((int)Field(typeof(Point), "Y").GetValue(boxedPoint) == 6);

        // Fields of a closed generic type, whose declaring type is closed, are read through the
        // instantiation's own handle.
        Check((int)Field(typeof(Gen<int>), "Value").GetValue(new Gen<int> { Value = 8 }) == 8);
        Check((string)Field(typeof(Gen<string>), "Value").GetValue(new Gen<string> { Value = "s" }) == "s");

        // A field of an open generic definition is refused by the managed accessor before any
        // primitive runs.
        try
        {
            Field(typeof(Gen<>), "Value").GetValue(null);
            Check(false);
        }
        catch (InvalidOperationException)
        {
            Check(true);
        }

        // A static field on a type whose initialiser has not yet run: the read runs it first, so
        // the initialiser's 7 is what comes back rather than the slot's initial 0.
        Check((int)Field(typeof(LazyHolder), "Total").GetValue(null) == 7);

        // Static storage nothing has ever written answers the field type's default.
        Check((int)Field(typeof(Untouched), "Never").GetValue(null) == 0);
        Check(Field(typeof(Untouched), "NeverSet").GetValue(null) == null);

        // A [ThreadStatic] field reads the calling thread's slot. This is the one field kind for
        // which CoreCLR itself reports IsFastPathSupported = false, so both runtimes reach the
        // QCall by the same managed route.
        ThreadStaticHolder.PerThread = 11;
        Check((int)Field(typeof(ThreadStaticHolder), "PerThread").GetValue(null) == 11);

        // Static readonly fields of reference and struct type.
        Check((string)Field(typeof(ReadOnlyHolder), "Name").GetValue(null) == "fixed");
        object origin = Field(typeof(ReadOnlyHolder), "Origin").GetValue(null);
        Check(((Point)origin).X == 3 && ((Point)origin).Y == 4);

        // Repeat reads go through a differently-configured managed accessor (the first call
        // reports the class as initialised, which switches FieldAccessor off its first-call
        // state), so exercise the second one too, and confirm it sees a fresh write.
        target.Number = -3;
        Check((int)Field(typeof(Target), "Number").GetValue(target) == -3);

        // A reflective write followed by a reflective read of the same field round-trips.
        Field(typeof(Target), "Number").SetValue(target, 99);
        Check((int)Field(typeof(Target), "Number").GetValue(target) == 99);

        return firstFailure;
    }
}
