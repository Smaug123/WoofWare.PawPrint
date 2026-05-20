using System;

class Box<T> { }

public class Program
{
    public static int Main(string[] args)
    {
        // On .NET 10 this flows through RuntimeType.IsAssignableFrom →
        // RuntimeTypeHandle.CanCastTo → TypeHandle.CanCastTo. The source is a
        // TypeDesc-shaped open generic definition, which lands in
        // TypeHandle_CanCastTo_NoCacheLookup with a non-Closed
        // RuntimeTypeHandleTarget. PawPrint's QCall handler currently TODO-fails
        // on non-Closed handles; CoreCLR returns true here because every
        // managed type (open generic definitions included) derives from object.
        if (!typeof(object).IsAssignableFrom(typeof(Box<>))) return 1;
        return 0;
    }
}
