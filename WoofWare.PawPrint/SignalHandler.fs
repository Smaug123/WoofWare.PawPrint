namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Identity of a managed callback the BCL handed System.Native's signal code
/// as an unmanaged function pointer: `OnPosixSignal` through
/// `SystemNative_SetPosixSignalHandler`, or one of System.Diagnostics.Process's
/// SIGCHLD callbacks. Wraps the `MethodInfo` of the target so that a call of it
/// has the call site pre-resolved — no need to round-trip through raw pointer
/// bits.
///
/// The wrapper exists purely so `PosixSignalShim` keeps clean structural
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
            LocalTypes : ImmutableArray<ConcreteTypeHandle>
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
    /// The callback `mi`, whose locals concretise to `localTypes`
    /// (`ExecutionConcretization.concretizeLocals`). They are concretised when the
    /// callback is installed rather than when it is called: its frame is pushed by
    /// `SignalDispatch.poll`, outside any step, where loading an assembly could not
    /// be announced to an `AppDomain.AssemblyLoad` handler.
    let ofMethodInfo
        (mi : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (localTypes : ImmutableArray<ConcreteTypeHandle>)
        : SignalHandler
        =
        {
            Method = mi
            LocalTypes = localTypes
        }

    let methodInfo (handler : SignalHandler) : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        handler.Method

    /// The concrete types of the callback's locals, as `ofMethodInfo` was given them.
    let localTypes (handler : SignalHandler) : ImmutableArray<ConcreteTypeHandle> = handler.LocalTypes
