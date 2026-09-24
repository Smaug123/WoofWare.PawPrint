using System;
using System.Reflection;

// FieldInfo.GetValue and SetValue on fields of pointer and function-pointer type. Reflection does
// not pass a pointer through as itself:
//
//  * `InvokeUtil::GetFieldValue` wraps a pointer field's value in a `System.Reflection.Pointer`
//    whose recorded type is the *field's* pointer type (`int*`, not `int`), and boxes a
//    function-pointer field's value as an `IntPtr`;
//  * on the way in, `RuntimeType.CheckValue` turns a `Pointer` into an `IntPtr` after checking its
//    recorded type against the field's, and `InvokeUtil::SetValidField` stores the `IntPtr`'s bits.
//
// Every pointer here addresses storage that outlives the reads, and each one is dereferenced after
// its round trip through reflection, so the address must survive the trip intact.
//
// `SetValue(null)` on a function-pointer field is deliberately absent. Once a first access has
// initialised real .NET's `FieldAccessor`, a function-pointer field takes the `IntPtr` fast path,
// which reads the null value's payload and throws `NullReferenceException`. PawPrint reports no
// fast path, so it stays on the `RuntimeFieldHandle_SetValue` QCall and stores a null pointer, as
// CoreCLR's own slow path does.
unsafe struct PointerPair
{
    public int* First;
    public long Tag;
}

unsafe class Holder
{
    public int* IntPtrField;
    public int* Unset;
    public void* Untyped;
    public long* Wide;
    public delegate*<int, int> Doubler;
    public PointerPair Pair;

    public static int* StaticPtr;
    public static delegate*<int, int> StaticDoubler;
}

public unsafe class Program
{
    private static int s_cell = 17;
    private static int s_other = 5;
    private static long s_wide = 1L << 40;

    private static int Twice (int x)
    {
        return 2 * x;
    }

    private static int Thrice (int x)
    {
        return 3 * x;
    }

    private static FieldInfo Field (string name)
    {
        FieldInfo f = typeof (Holder).GetField (
            name,
            BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public);

        if (f == null)
            throw new Exception ("could not find " + name);

        return f;
    }

    public static int Main (string[] args)
    {
        Holder h = new Holder ();

        fixed (int* cell = &s_cell)
        fixed (int* other = &s_other)
        fixed (long* wide = &s_wide)
        {
            h.IntPtrField = cell;

            // An instance pointer field answers a `Pointer`, never the bare address or an `IntPtr`.
            object read = Field ("IntPtrField").GetValue (h);

            if (read == null)
                return 1;

            if (read.GetType () != typeof (Pointer))
                return 2;

            int* back = (int*)Pointer.Unbox (read);

            if (back != cell || *back != 17)
                return 3;

            // Writing through the pointer that came back lands in the original storage.
            *back = 19;

            if (s_cell != 19)
                return 4;

            // Reading the same field again answers a fresh `Pointer` around the same address.
            object readAgain = Field ("IntPtrField").GetValue (h);

            if (ReferenceEquals (read, readAgain))
                return 5;

            if ((int*)Pointer.Unbox (readAgain) != cell)
                return 6;

            // A null pointer field still answers a non-null `Pointer`, around null.
            object unset = Field ("Unset").GetValue (h);

            if (!(unset is Pointer))
                return 7;

            if (Pointer.Unbox (unset) != null)
                return 8;

            // The `Pointer` records the pointer type `int*`: `SetValue` accepts it into another
            // `int*` field and into a `void*` field, ...
            Field ("Unset").SetValue (h, read);

            if (h.Unset != cell || *h.Unset != 19)
                return 9;

            Field ("Untyped").SetValue (h, read);

            if (h.Untyped != cell || *(int*)h.Untyped != 19)
                return 10;

            // ... and refuses it into a `long*` field.
            try
            {
                Field ("Wide").SetValue (h, read);
                return 11;
            }
            catch (ArgumentException)
            {
            }

            if (h.Wide != null)
                return 12;

            // A `void*` field's `Pointer` records `void*`, and dereferences to the same storage.
            object untyped = Field ("Untyped").GetValue (h);

            if (!(untyped is Pointer))
                return 13;

            if (*(int*)Pointer.Unbox (untyped) != 19)
                return 14;

            // `void*` does not convert to `int*`, so that `Pointer` is refused by an `int*` field.
            try
            {
                Field ("Unset").SetValue (h, untyped);
                return 15;
            }
            catch (ArgumentException)
            {
            }

            // `SetValue` accepts an `IntPtr` for a pointer field ...
            Field ("Wide").SetValue (h, (IntPtr)wide);

            if (h.Wide != wide || *h.Wide != 1L << 40)
                return 16;

            // ... a `UIntPtr` only for a `void*` field, which accepts a value of any type that
            // `CheckValue` can turn into a pointer ...
            Field ("Untyped").SetValue (h, (UIntPtr)other);

            if (h.Untyped != other || *(int*)h.Untyped != 5)
                return 17;

            try
            {
                Field ("Unset").SetValue (h, (UIntPtr)other);
                return 18;
            }
            catch (ArgumentException)
            {
            }

            // ... a `Pointer` built by `Pointer.Box` of the field's own type ...
            Field ("IntPtrField").SetValue (h, Pointer.Box (other, typeof (int*)));

            if (h.IntPtrField != other || *h.IntPtrField != 5)
                return 19;

            // ... and a null, which clears it.
            Field ("IntPtrField").SetValue (h, null);

            if (h.IntPtrField != null)
                return 20;

            // A static pointer field.
            Holder.StaticPtr = other;
            object staticRead = Field ("StaticPtr").GetValue (null);

            if (!(staticRead is Pointer))
                return 21;

            if (*(int*)Pointer.Unbox (staticRead) != 5)
                return 22;

            Field ("StaticPtr").SetValue (null, (IntPtr)cell);

            if (Holder.StaticPtr != cell)
                return 23;

            // A pointer field of a struct, read out of a box of that struct ...
            PointerPair pair = new PointerPair ();
            pair.First = cell;
            pair.Tag = 3;
            object boxedPair = pair;

            FieldInfo first = typeof (PointerPair).GetField ("First");
            object firstRead = first.GetValue (boxedPair);

            if (!(firstRead is Pointer))
                return 24;

            if (*(int*)Pointer.Unbox (firstRead) != 19)
                return 25;

            // ... and written into the box, which a later unbox sees.
            first.SetValue (boxedPair, Pointer.Box (other, typeof (int*)));

            if (((PointerPair)boxedPair).First != other)
                return 26;

            // A struct-typed field whose own field is a pointer is read as a box of the struct,
            // and the pointer inside that box is intact.
            h.Pair = pair;
            object pairRead = Field ("Pair").GetValue (h);

            if (!(pairRead is PointerPair pairValue))
                return 27;

            if (pairValue.First != cell || *pairValue.First != 19)
                return 28;
        }

        // A function-pointer field answers a boxed `IntPtr`, which is still callable.
        Holder fn = new Holder ();
        fn.Doubler = &Twice;

        object fnRead = Field ("Doubler").GetValue (fn);

        if (!(fnRead is IntPtr fnValue))
            return 29;

        if (((delegate*<int, int>)fnValue) (21) != 42)
            return 30;

        // A null function-pointer field answers a boxed `IntPtr.Zero`, not null.
        object fnUnset = Field ("StaticDoubler").GetValue (null);

        if (!(fnUnset is IntPtr fnUnsetValue) || fnUnsetValue != IntPtr.Zero)
            return 31;

        // `SetValue` on a function-pointer field takes an `IntPtr`.
        delegate*<int, int> thrice = &Thrice;
        Field ("StaticDoubler").SetValue (null, (IntPtr)thrice);

        if (Holder.StaticDoubler (7) != 21)
            return 32;

        return 0;
    }
}
