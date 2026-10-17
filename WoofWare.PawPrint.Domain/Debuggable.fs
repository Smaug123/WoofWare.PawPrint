namespace WoofWare.PawPrint

open System.Collections.Immutable

/// <summary>
/// What an assembly's <c>[assembly: Debuggable]</c> says about whether CoreCLR's JIT may optimise
/// the assembly's code.
/// </summary>
/// <remarks>
/// CoreCLR reads the attribute when it loads the assembly (<c>Assembly::ComputeDebuggingConfig</c>,
/// <c>vm/assembly.cpp</c>), and compiles the code of a module whose attribute disallows optimisation
/// as debuggable code (<c>GetDebuggerCompileFlags</c>, <c>vm/jitinterface.cpp</c>). C# emits an
/// attribute that disallows it for a Debug build. A debugger or a profiler can disallow it too,
/// which nothing in the assembly says.
/// </remarks>
[<RequireQualifiedAccess>]
module Debuggable =

    /// `Assembly::GetDebuggingCustomAttributes` (`vm/assembly.cpp`), which reads only a blob of six
    /// or eight bytes, the size of the attribute's `(bool, bool)` and `(DebuggingModes)`
    /// constructors' arguments: one whose prolog is not `01 00` fails the assembly's load (`None`),
    /// and otherwise optimisation is disallowed when the low bit of its third byte and its whole
    /// fourth byte are both set. For the `(DebuggingModes)` constructor, those are `Default` and the
    /// low byte of `DisableOptimizations` shifted down. A blob of any other size leaves
    /// optimisation allowed.
    let internal blobAllowsOptimisation (blob : ImmutableArray<byte>) : bool option =
        if blob.Length <> 6 && blob.Length <> 8 then Some true
        elif blob.[0] <> 1uy || blob.[1] <> 0uy then None
        else Some ((blob.[2] &&& 1uy) = 0uy || blob.[3] = 0uy)

    /// The attribute's type, by the name CoreCLR's `GetCustomAttributeByName` looks for.
    [<Literal>]
    let private AttributeName = "System.Diagnostics.DebuggableAttribute"

    /// Whether CoreCLR's JIT may optimise the code of `assembly`, as far as the assembly says:
    /// decided, as CoreCLR decides it, by the first of the assembly's attributes whose type is named
    /// <c>System.Diagnostics.DebuggableAttribute</c>, however that name is split between namespace
    /// and name and whatever assembly it is scoped to. `None` where that attribute is malformed, so
    /// that CoreCLR does not load the assembly at all.
    let allowsJitOptimisation (assembly : DumpedAssembly) : bool option =
        match NamedAttribute.ofAssembly AttributeName assembly with
        | [] -> Some true
        | attribute :: _ -> blobAllowsOptimisation attribute.Value
