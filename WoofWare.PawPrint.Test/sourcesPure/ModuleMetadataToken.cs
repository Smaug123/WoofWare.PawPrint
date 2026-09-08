using System;
using System.Reflection;

public static class Program
{
    // `Module.MetadataToken` is `ModuleHandle.GetToken`, the `ModuleHandle_GetToken` QCall,
    // which is `IMDInternalImport::GetModuleFromScope`: the token of the Module table's one row,
    // `TokenFromRid(1, mdtModule)` (mdinternalro.cpp:3142). So it is 0x00000001 for every module
    // that has metadata at all, in every assembly.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        Module own = typeof(Program).Module;
        int token = own.MetadataToken;

        // Table 0x00 is the Module table, and the row is the first one.
        if ((token >> 24) != 0x00) return 1;
        if ((token & 0x00FFFFFF) != 1) return 2;
        if (token != 0x00000001) return 3;

        // The manifest module of the same assembly is the same module, so the same token.
        if (typeof(Program).Assembly.ManifestModule.MetadataToken != token) return 4;

        // Another assembly's module answers the same, because every Module table has one row.
        if (typeof(int).Module.MetadataToken != 0x00000001) return 5;
        if (typeof(Uri).Module.MetadataToken != 0x00000001) return 6;

        // The token is a fact about the module, not about which type asked.
        if (typeof(string).Module.MetadataToken != typeof(int).Module.MetadataToken) return 7;

        return 0;
    }
}
