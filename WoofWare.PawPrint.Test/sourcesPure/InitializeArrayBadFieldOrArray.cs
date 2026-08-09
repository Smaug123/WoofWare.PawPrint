// The three rejections `RuntimeHelpers.InitializeArray` makes after its two argument checks,
// in the order CoreCLR makes them (RuntimeHelpers.CoreCLR.cs):
//
//     if (!RuntimeFieldHandle.GetRVAFieldInfo(fldInfo.Value, out void* address, out uint size))
//         throw new ArgumentException(SR.Argument_BadFieldForInitializeArray);
//     ...
//     if (elementTH.IsTypeDesc || !elementTH.AsMethodTable()->IsPrimitive) // Enum is included
//         throw new ArgumentException(SR.Argument_BadArrayForInitializeArray);
//     nuint totalSize = pMT->ComponentSize * array.NativeLength;
//     // make certain you don't go off the end of the rva static
//     if (totalSize > size)
//         throw new ArgumentException(SR.Argument_BadFieldForInitializeArray);
//
// Both messages are constants with nothing interpolated, so they can be asserted exactly.
// The element-type check runs *before* the size check, which the `string[]` case below pins:
// that array is both non-primitive and far too big for the field, and must report the former.
//
// Reaching a real RVA field from C# needs the `<PrivateImplementationDetails>` type Roslyn
// synthesises for the array initialiser; `InitializeArrayBoxedFieldHandle.cs` uses the same
// route. Keep exactly one array literal in this file so the "size 12" field is unambiguous.

using System;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace InitializeArrayBadFieldOrArrayTest
{
    class Program
    {
        private static readonly int[] Seed = new[] { 173, 257, 409 };

        private static int NoRvaStatic = 7;

        private static FieldInfo FindField(Type type, Func<FieldInfo, bool> predicate)
        {
            FieldInfo found = null;

            foreach (FieldInfo candidate in type.GetFields(
                BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static))
            {
                if (predicate(candidate))
                {
                    if (found != null)
                    {
                        return null;
                    }

                    found = candidate;
                }
            }

            return found;
        }

        static int Main(string[] args)
        {
            Type privateImplementationDetails = typeof(Program).Assembly.GetType("<PrivateImplementationDetails>");
            if (privateImplementationDetails == null)
            {
                return 1;
            }

            FieldInfo dataField = FindField(
                privateImplementationDetails,
                f => f.FieldType.Name.Contains("12", StringComparison.Ordinal));

            if (dataField == null)
            {
                return 2;
            }

            FieldInfo noRvaField = FindField(typeof(Program), f => f.Name == "NoRvaStatic");
            if (noRvaField == null)
            {
                return 3;
            }

            // Positive control: the field handle is usable, so the rejections below are
            // attributable to the array argument rather than to the field.
            int[] ok = new int[3];
            RuntimeHelpers.InitializeArray(ok, dataField.FieldHandle);
            if (ok[0] != Seed[0] || ok[1] != Seed[1] || ok[2] != Seed[2])
            {
                return 4;
            }

            // A field with no RVA: GetRVAFieldInfo returns FALSE.
            try
            {
                RuntimeHelpers.InitializeArray(new int[1], noRvaField.FieldHandle);
                return 5;
            }
            catch (Exception e) when (e.GetType() == typeof(ArgumentException))
            {
                if (e.Message != "The field is invalid for initializing array or span.")
                {
                    return 6;
                }
            }

            // A non-primitive element type. Also oversized for the 12-byte field, so the
            // message discriminates which of the two checks fired first.
            try
            {
                RuntimeHelpers.InitializeArray(new string[3], dataField.FieldHandle);
                return 7;
            }
            catch (Exception e) when (e.GetType() == typeof(ArgumentException))
            {
                if (e.Message != "Only array or span of primitive or enum types can be initialized from static data.")
                {
                    return 8;
                }
            }

            // Primitive elements, but 16 bytes wanted from a 12-byte field.
            try
            {
                RuntimeHelpers.InitializeArray(new int[4], dataField.FieldHandle);
                return 9;
            }
            catch (Exception e) when (e.GetType() == typeof(ArgumentException))
            {
                if (e.Message != "The field is invalid for initializing array or span.")
                {
                    return 10;
                }
            }

            // Exactly the field's size is fine, and a shorter array is too.
            int[] shorter = new int[2];
            RuntimeHelpers.InitializeArray(shorter, dataField.FieldHandle);
            if (shorter[0] != Seed[0] || shorter[1] != Seed[1])
            {
                return 11;
            }

            return 0;
        }
    }
}
