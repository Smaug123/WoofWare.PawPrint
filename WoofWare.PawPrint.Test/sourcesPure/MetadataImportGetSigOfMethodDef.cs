using System;

namespace MetadataImportGetSigOfMethodDef
{
    [AttributeUsage(AttributeTargets.Class)]
    public sealed class MarkerAttribute : Attribute { }

    [Marker]
    public class Decorated { }

    public class Plain { }

    public class Program
    {
        public static int Main(string[] args)
        {
            // Attribute.IsDefined goes through RuntimeCustomAttributeData.IsCustomAttributeDefined,
            // which calls scope.GetMethodSignature(caCtorToken) to read the attribute ctor's
            // signature. For a MethodDef token (the [Marker] ctor lives in this assembly), the
            // CoreLib path calls MetadataImport.GetSigOfMethodDef. We test both decorated and
            // undecorated targets so we exercise both the "found" and "not found" code paths
            // that nonetheless inspect candidate constructors via this InternalCall.
            if (!Attribute.IsDefined(typeof(Decorated), typeof(MarkerAttribute))) return 1;
            if (Attribute.IsDefined(typeof(Plain), typeof(MarkerAttribute))) return 2;

            return 0;
        }
    }
}
