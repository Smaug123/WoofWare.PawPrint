// Where a pinned byref actually sits modulo 8 and 16, and how that relates to
// the object pointer it came from.
//
// PawPrint models a byref as an unknown container base with some guaranteed low
// zero bits, plus a known byte offset. This probe measures which base that
// should be: an object pointer is 8-byte aligned on 64-bit, and a string's char
// data sits a *fixed* 12 bytes past it, so a string char pointer is 4 mod 8 —
// never 8-byte aligned. Modelling the data start as "4-byte aligned" instead
// loses exactly the bit that `UnicodeEncoding.GetByteCount`'s FASTLOOP gate
// asks about.
//
// `addr & 15` is deliberately reported too, and is *not* determined: objects
// are 8-byte aligned, not 16, so both 4 and 12 occur for a string. That is the
// boundary of what any model without a simulated address space can answer.
//
//     dotnet run --project AlignmentProbe.csproj
//     dotnet publish -r linux-x64 --self-contained -c Release -o pub
//     container run --rm --arch amd64 -e DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 \
//         -v "$PWD/pub:/probe" debian:stable-slim /probe/AlignmentProbe

using System;
using System.Runtime.CompilerServices;

unsafe class AlignmentProbe
{
    const int Samples = 200;

    static string Join(int[] xs)
    {
        // Hand-rolled: `string.Join` reaches CultureInfo, which FailFasts in a
        // container with no ICU.
        var sb = new System.Text.StringBuilder();
        for (int i = 0; i < xs.Length; i++) { if (i > 0) sb.Append(','); sb.Append(xs[i]); }
        return sb.ToString();
    }

    static void Row(string label, long addr, long objAddr)
        => Console.WriteLine($"{label,-32} addr&7={addr & 7} addr&15={addr & 15} obj&7={objAddr & 7} obj&15={objAddr & 15} delta={addr - objAddr}");

    static long ObjectAddress<T>(T o) where T : class => (long)*(IntPtr*)Unsafe.AsPointer(ref o);

    static void Main()
    {
#pragma warning disable SYSLIB0057, CS0618 // OffsetToStringData is the constant under measurement
        Console.WriteLine($"OffsetToStringData={RuntimeHelpers.OffsetToStringData} IntPtr.Size={IntPtr.Size}");
#pragma warning restore SYSLIB0057, CS0618

        var stringMod8 = new int[8];
        var stringMod16 = new int[16];
        var objMod8 = new int[8];
        for (int len = 0; len < Samples; len++)
        {
            string s = new string('x', len);
            long obj = ObjectAddress(s);
            objMod8[(int)(obj & 7)]++;
            fixed (char* p = s)
            {
                stringMod8[(int)((long)p & 7)]++;
                stringMod16[(int)((long)p & 15)]++;
                if (len < 3) Row($"string len {len}, &s[0]", (long)p, obj);
                if (len == 4) { Row("string len 4, &s[1]", (long)(p + 1), obj); Row("string len 4, &s[2]", (long)(p + 2), obj); }
            }
        }
        Console.WriteLine("object pointer     over &7:  " + Join(objMod8));
        Console.WriteLine("string &s[0]       over &7:  " + Join(stringMod8));
        Console.WriteLine("string &s[0]       over &15: " + Join(stringMod16));

        var charArrayMod8 = new int[8];
        var byteArrayMod8 = new int[8];
        for (int len = 0; len < Samples; len++)
        {
            char[] a = new char[len];
            fixed (char* p = a) { charArrayMod8[(int)((long)p & 7)]++; if (len == 1) Row("char[1], &a[0]", (long)p, ObjectAddress(a)); }
            byte[] b = new byte[len];
            fixed (byte* p = b) { byteArrayMod8[(int)((long)p & 7)]++; if (len == 1) Row("byte[1], &b[0]", (long)p, ObjectAddress(b)); }
        }
        Console.WriteLine("char[] &a[0]       over &7:  " + Join(charArrayMod8));
        Console.WriteLine("byte[] &b[0]       over &7:  " + Join(byteArrayMod8));

        // A string past the large-object threshold, in case the LOH aligns differently.
        string big = new string('y', 60000);
        fixed (char* p = big) Row("LOH string len 60000, &s[0]", (long)p, ObjectAddress(big));

        // What UnicodeEncoding.GetByteCount's FASTLOOP gate decides, which is
        // the question that sent us here.
        int aligned = 0;
        for (int len = 0; len < Samples; len++)
        {
            string s = new string('z', len);
            fixed (char* p = s) if (((long)p & 7) == 0) aligned++;
        }
        Console.WriteLine($"of {Samples} strings, ((long)&s[0] & 7) == 0 for {aligned}");
    }
}
