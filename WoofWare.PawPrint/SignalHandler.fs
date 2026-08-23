namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// Identity of the managed callback the BCL installed via
/// `SystemNative_SetPosixSignalHandler(&OnPosixSignal)`. Wraps the
/// `MethodInfo` of the target so a later signal-delivery slice has the
/// call site pre-resolved — no need to round-trip through raw pointer
/// bits.
///
/// The wrapper exists purely so `SignalState` keeps clean structural
/// equality: `MethodInfo<_,_,_>` carries `ImmutableArray` fields and a
/// `MethodBody` DU whose payloads use reference equality, so naked
/// `MethodInfo` equality is unstable. `MethodInfo.NominallyEqual` is the
/// stable identity contract (assembly + type identity + type generics +
/// method handle + method generics) and is exactly what we need here:
/// two `SignalHandler`s denote the same callback iff they would dispatch
/// to the same managed method. Mirrors the same pattern used by
/// `NativeIntSource.FunctionPointer`'s custom equality at the eval-stack
/// layer.
[<CustomEquality>]
[<NoComparison>]
type SignalHandler =
    private
        {
            Method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        }

    member this.MethodInfo : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        this.Method

    override this.Equals (other : obj) : bool =
        match other with
        | :? SignalHandler as other -> MethodInfo.NominallyEqual this.Method other.Method
        | _ -> false

    override this.GetHashCode () : int =
        hash (this.Method.Owner, this.Method.IdentityKey, this.Method.Generics)

[<RequireQualifiedAccess>]
module SignalHandler =
    let ofMethodInfo (mi : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) : SignalHandler =
        {
            Method = mi
        }

    let methodInfo (handler : SignalHandler) : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        handler.Method
