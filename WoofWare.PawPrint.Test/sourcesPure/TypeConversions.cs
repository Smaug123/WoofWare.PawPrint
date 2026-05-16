using System;

public class TestTypeConversions
{
    // Test Conv_I: Convert to native int
    public static int TestConvI()
    {
        // From int32
        int i32 = 42;
        IntPtr nativeFromI32 = (IntPtr)i32;
        if (nativeFromI32.ToInt64() != 42L) return 1;
        
        // From int64
        long i64 = 1234567890L;
        IntPtr nativeFromI64 = (IntPtr)i64;
        if (nativeFromI64.ToInt64() != 1234567890L) return 2;
        
        // From negative
        int neg = -100;
        IntPtr nativeFromNeg = (IntPtr)neg;
        if (nativeFromNeg.ToInt64() != -100L) return 3;
        
        return 0;
    }
    
    // Test Conv_I1: Convert to int8 (sbyte)
    public static int TestConvI1()
    {
        // From int32
        int i = 100;
        sbyte b = (sbyte)i;
        if (b != 100) return 10;
        
        // Truncation
        i = 300; // Exceeds sbyte range
        b = unchecked((sbyte)i);
        if (b != 44) return 11; // 300 & 0xFF = 44
        
        // Negative
        i = -50;
        b = (sbyte)i;
        if (b != -50) return 12;
        
        // From larger type
        long l = 127L;
        b = (sbyte)l;
        if (b != 127) return 13;
        
        return 0;
    }
    
    // Test Conv_I2: Convert to int16 (short)
    public static int TestConvI2()
    {
        // From int32
        int i = 1000;
        short s = (short)i;
        if (s != 1000) return 20;
        
        // Truncation
        i = 70000; // Exceeds short range
        s = unchecked((short)i);
        if (s != 4464) return 21; // 70000 & 0xFFFF = 4464
        
        // Negative
        i = -1000;
        s = (short)i;
        if (s != -1000) return 22;
        
        // From byte
        byte b = 200;
        s = (short)b;
        if (s != 200) return 23;
        
        return 0;
    }
    
    // Test Conv_I4: Convert to int32
    public static int TestConvI4()
    {
        // From long
        long l = 42L;
        int i = (int)l;
        if (i != 42) return 30;
        
        // Truncation from long
        l = 0x1_0000_0000L; // Exceeds int32
        i = unchecked((int)l);
        if (i != 0) return 31;
        
        // From byte
        byte b = 255;
        i = (int)b;
        if (i != 255) return 32;
        
        // From short
        short s = -1000;
        i = (int)s;
        if (i != -1000) return 33;
        
        // From float (truncates)
        float f = 3.14f;
        i = (int)f;
        if (i != 3) return 34;
        
        return 0;
    }
    
    // Test Conv_I8: Convert to int64
    public static int TestConvI8()
    {
        // From int32
        int i = 1000000;
        long l = (long)i;
        if (l != 1000000L) return 40;
        
        // From negative int32
        i = -1000000;
        l = (long)i;
        if (l != -1000000L) return 41;
        
        // From byte (zero extends)
        byte b = 255;
        l = (long)b;
        if (l != 255L) return 42;
        
        // From short
        short s = -32000;
        l = (long)s;
        if (l != -32000L) return 43;
        
        // From float
        float f = 1e10f;
        l = (long)f;
        if (l != 10000000000L) return 44;
        
        return 0;
    }
    
    // Test Conv_R4: Convert to float32
    public static int TestConvR4()
    {
        // From int32
        int i = 42;
        float f = (float)i;
        if (f != 42.0f) return 50;
        
        // From long
        long l = 1000000L;
        f = (float)l;
        if (f != 1000000.0f) return 51;
        
        // From negative
        i = -100;
        f = (float)i;
        if (f != -100.0f) return 52;
        
        // Large values may lose precision
        i = 16777217; // First int that can't be exactly represented
        f = (float)i;
        float expected = 16777216.0f; // Rounds to nearest even
        if (f != expected) return 53;
        
        return 0;
    }
    
    // Test Conv_R8: Convert to float64 (double)
    public static int TestConvR8()
    {
        // From int32
        int i = 42;
        double d = (double)i;
        if (d != 42.0) return 60;
        
        // From long
        long l = 1000000000000L;
        d = (double)l;
        if (d != 1000000000000.0) return 61;
        
        // From float
        float f = 3.14f;
        d = (double)f;
        if (Math.Abs(d - 3.14f) > 0.00001) return 62;
        
        // From negative
        i = -12345;
        d = (double)i;
        if (d != -12345.0) return 63;
        
        return 0;
    }
    
    // Test Conv_U: Convert to unsigned native int
    public static int TestConvU()
    {
        // From positive int32
        int i = 42;
        UIntPtr uNative = (UIntPtr)(uint)i;
        if (uNative.ToUInt64() != 42UL) return 70;
        
        // From uint32
        uint u = 0xFFFFFFFF;
        uNative = (UIntPtr)u;
        if (uNative.ToUInt64() != 0xFFFFFFFFUL) return 71;
        
        // From long
        long l = 1234567890L;
        uNative = (UIntPtr)(ulong)l;
        if (uNative.ToUInt64() != 1234567890UL) return 72;
        
        return 0;
    }
    
    // Test Conv_U1: Convert to uint8 (byte)
    public static int TestConvU1()
    {
        // From int32
        int i = 200;
        byte b = (byte)i;
        if (b != 200) return 80;
        
        // Truncation
        i = 300;
        b = (byte)i;
        if (b != 44) return 81; // 300 & 0xFF
        
        // From negative (wraps)
        i = -1;
        b = unchecked((byte)i);
        if (b != 255) return 82;
        
        // From larger type
        long l = 100L;
        b = (byte)l;
        if (b != 100) return 83;
        
        return 0;
    }
    
    // Test Conv_U2: Convert to uint16 (ushort)
    public static int TestConvU2()
    {
        // From int32
        int i = 50000;
        ushort us = (ushort)i;
        if (us != 50000) return 90;
        
        // Truncation
        i = 70000;
        us = unchecked((ushort)i);
        if (us != 4464) return 91; // 70000 & 0xFFFF
        
        // From byte
        byte b = 255;
        us = (ushort)b;
        if (us != 255) return 92;
        
        // From negative (wraps)
        i = -1;
        us = unchecked((ushort)i);
        if (us != 65535) return 93;
        
        return 0;
    }
    
    // Test Conv_U4: Convert to uint32
    public static int TestConvU4()
    {
        // From long
        long l = 1000000L;
        uint u = (uint)l;
        if (u != 1000000U) return 100;
        
        // From negative int (reinterpret)
        int i = -1;
        u = unchecked((uint)i);
        if (u != 0xFFFFFFFF) return 101;
        
        // From byte
        byte b = 255;
        u = (uint)b;
        if (u != 255U) return 102;
        
        // From ushort
        ushort us = 65535;
        u = (uint)us;
        if (u != 65535U) return 103;
        
        return 0;
    }
    
    // Test Conv_U8: Convert to uint64 (ulong)
    public static int TestConvU8()
    {
        // From uint32
        uint u = 0xFFFFFFFF;
        ulong ul = (ulong)u;
        if (ul != 0xFFFFFFFFUL) return 110;
        
        // From int32 (sign extends first)
        int i = -1;
        ul = unchecked((ulong)i);
        if (ul != 0xFFFFFFFFFFFFFFFFUL) return 111;
        
        // From positive int32
        i = 1000000;
        ul = (ulong)i;
        if (ul != 1000000UL) return 112;
        
        // From byte
        byte b = 255;
        ul = (ulong)b;
        if (ul != 255UL) return 113;
        
        return 0;
    }
    
    // Test overflow conversions
    public static int TestConvOverflow()
    {
        // Conv_ovf_i4: Convert to int32 with overflow check
        try
        {
            long l = (long)int.MaxValue + 1;
            int i = checked((int)l);
            return 120; // Should have thrown
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Conv_ovf_u4: Convert to uint32 with overflow check
        try
        {
            long l = -1;
            uint u = checked((uint)l);
            return 121; // Should have thrown
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Conv_ovf_i1: Convert to sbyte with overflow check
        try
        {
            int i = 200; // Exceeds sbyte.MaxValue
            sbyte b = checked((sbyte)i);
            return 122; // Should have thrown
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Valid checked conversion
        try
        {
            int i = 100;
            sbyte b = checked((sbyte)i);
            if (b != 100) return 123;
        }
        catch (OverflowException)
        {
            return 124; // Should not throw
        }
        
        return 0;
    }
    
    // Test unsigned overflow conversions
    public static int TestConvOverflowUnsigned()
    {
        // Conv_ovf_u1_un: Unsigned to byte with overflow check
        try
        {
            uint u = 256;
            byte b = checked((byte)u);
            return 130; // Should have thrown
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Conv_ovf_i4_un: Unsigned to int32 with overflow check
        try
        {
            uint u = (uint)int.MaxValue + 1;
            int i = checked((int)u);
            return 131; // Should have thrown
        }
        catch (OverflowException)
        {
            // Expected
        }
        
        // Valid unsigned conversion
        try
        {
            uint u = 100;
            byte b = checked((byte)u);
            if (b != 100) return 132;
        }
        catch (OverflowException)
        {
            return 133; // Should not throw
        }
        
        return 0;
    }
    
    // Test checked conversions from floating-point sources
    public static int TestConvOverflowFromFloat()
    {
        // Valid checked float-to-int truncates toward zero
        try
        {
            double d = 3.7;
            int i = checked((int)d);
            if (i != 3) return 150;
        }
        catch (OverflowException)
        {
            return 151;
        }

        try
        {
            double d = -3.7;
            int i = checked((int)d);
            if (i != -3) return 152;
        }
        catch (OverflowException)
        {
            return 153;
        }

        // Out-of-range float to int overflows
        try
        {
            double d = 1e20;
            int i = checked((int)d);
            return 154;
        }
        catch (OverflowException)
        {
            // Expected
        }

        // NaN to checked int overflows
        try
        {
            double d = double.NaN;
            int i = checked((int)d);
            return 155;
        }
        catch (OverflowException)
        {
            // Expected
        }

        // Negative float to checked uint overflows
        try
        {
            double d = -1.5;
            uint u = checked((uint)d);
            return 156;
        }
        catch (OverflowException)
        {
            // Expected
        }

        // Valid checked float-to-byte
        try
        {
            double d = 200.5;
            byte b = checked((byte)d);
            if (b != 200) return 157;
        }
        catch (OverflowException)
        {
            return 158;
        }

        // Out-of-range float to byte overflows
        try
        {
            double d = 300.0;
            byte b = checked((byte)d);
            return 159;
        }
        catch (OverflowException)
        {
            // Expected
        }

        // Valid checked float-to-sbyte (negative)
        try
        {
            double d = -100.9;
            sbyte sb = checked((sbyte)d);
            if (sb != -100) return 160;
        }
        catch (OverflowException)
        {
            return 161;
        }

        // Out-of-range float to sbyte overflows
        try
        {
            double d = 200.0;
            sbyte sb = checked((sbyte)d);
            return 162;
        }
        catch (OverflowException)
        {
            // Expected
        }

        return 0;
    }

    // Test Conv_r_un: Unsigned to float conversion
    public static int TestConvRUn()
    {
        // Convert large unsigned to float
        uint u = 0xFFFFFFFF;
        float f = (float)u;
        if (f != 4294967296.0f) return 140; // Rounds up
        
        // Convert unsigned long to double
        ulong ul = 0xFFFFFFFFFFFFFFFF;
        double d = (double)ul;
        if (d != 18446744073709551616.0) return 141; // Rounds up
        
        // Normal range
        u = 1000000;
        f = (float)u;
        if (f != 1000000.0f) return 142;
        
        return 0;
    }
    
    public static int Main(string[] argv)
    {
        int result;
        
        result = TestConvI();
        if (result != 0) return 3000 + result;
        
        result = TestConvI1();
        if (result != 0) return 3100 + result;
        
        result = TestConvI2();
        if (result != 0) return 3200 + result;
        
        result = TestConvI4();
        if (result != 0) return 3300 + result;
        
        result = TestConvI8();
        if (result != 0) return 3400 + result;
        
        result = TestConvR4();
        if (result != 0) return 3500 + result;
        
        result = TestConvR8();
        if (result != 0) return 3600 + result;
        
        result = TestConvU();
        if (result != 0) return 3700 + result;
        
        result = TestConvU1();
        if (result != 0) return 3800 + result;
        
        result = TestConvU2();
        if (result != 0) return 3900 + result;
        
        result = TestConvU4();
        if (result != 0) return 4000 + result;
        
        result = TestConvU8();
        if (result != 0) return 4100 + result;
        
        result = TestConvOverflow();
        if (result != 0) return 4200 + result;
        
        result = TestConvOverflowUnsigned();
        if (result != 0) return 4300 + result;
        
        result = TestConvRUn();
        if (result != 0) return 4400 + result;

        result = TestConvOverflowFromFloat();
        if (result != 0) return 4500 + result;

        return 0;
    }
}