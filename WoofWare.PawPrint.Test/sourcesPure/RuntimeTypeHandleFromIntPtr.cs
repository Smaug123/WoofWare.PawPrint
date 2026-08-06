using System;

public class Program
{
    public static unsafe int Main(string[] args)
    {
        // CoreCLR's TypeHandle is a tagged pointer: bit 1 is set exactly when the
        // handle wraps a TypeDesc rather than a MethodTable. `int*` is
        // TypeDesc-shaped, so its handle carries that tag.
        //
        // RuntimeTypeHandle.FromIntPtr runs GetRuntimeTypeFromHandle, whose managed
        // IL branches on TypeHandle.IsTypeDesc (`handle & 2`) and then calls
        // TypeHandle.AsTypeDesc (`handle & ~2`) to reach TypeDesc::_exposedClassObject.
        // That mask preserves the whole address and clears the tag, so its result is
        // the target's TypeDesc pointer — an identity PawPrint does not yet model.
        IntPtr rawPointerType = RuntimeTypeHandle.ToIntPtr(typeof(int*).TypeHandle);
        Type roundTrippedPointerType = Type.GetTypeFromHandle(RuntimeTypeHandle.FromIntPtr(rawPointerType));
        if (roundTrippedPointerType != typeof(int*)) return 1;
        if (roundTrippedPointerType.Name != "Int32*") return 2;

        // The MethodTable-shaped sibling takes the other branch: IsTypeDesc is
        // false, and AsMethodTable is a plain cast with no mask at all.
        IntPtr rawInt = RuntimeTypeHandle.ToIntPtr(typeof(int).TypeHandle);
        Type roundTrippedInt = Type.GetTypeFromHandle(RuntimeTypeHandle.FromIntPtr(rawInt));
        if (roundTrippedInt != typeof(int)) return 3;

        // A byref type is TypeDesc-shaped too, by a different ConcreteTypeHandle case.
        IntPtr rawByref = RuntimeTypeHandle.ToIntPtr(typeof(int).MakeByRefType().TypeHandle);
        Type roundTrippedByref = Type.GetTypeFromHandle(RuntimeTypeHandle.FromIntPtr(rawByref));
        if (roundTrippedByref != typeof(int).MakeByRefType()) return 4;

        return 0;
    }
}
