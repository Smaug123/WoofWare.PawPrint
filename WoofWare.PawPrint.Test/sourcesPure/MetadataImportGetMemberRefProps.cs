using System;

namespace MetadataImportGetMemberRefProps
{
    [Obsolete]
    public class Decorated { }

    public class Plain { }

    public class Program
    {
        public static int Main(string[] args)
        {
            // The mirror of MetadataImportGetSigOfMethodDef.cs, with the attribute moved out of
            // this assembly. Attribute.IsDefined reaches RuntimeCustomAttributeData's
            // FilterCustomAttributeRecord, which calls scope.GetMethodSignature(caCtorToken) on
            // each candidate ctor. ObsoleteAttribute lives in corelib, so the ctor token is a
            // MemberRef and GetMethodSignature dispatches to MetadataImport.GetMemberRefProps
            // instead of GetSigOfMethodDef.
            if (!Attribute.IsDefined(typeof(Decorated), typeof(ObsoleteAttribute))) return 1;
            if (Attribute.IsDefined(typeof(Plain), typeof(ObsoleteAttribute))) return 2;

            return 0;
        }
    }
}
