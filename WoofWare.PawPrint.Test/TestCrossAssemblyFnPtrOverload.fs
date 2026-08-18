namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// Two overloads whose parameter types differ only in the `modopt`s that spell a *combined*
/// unmanaged calling convention. Called from the assembly that declares them, the call site is a
/// MethodDef token and no signature matching happens at all; called from another assembly it is a
/// MemberRef, and PawPrint has to pick which same-named MethodDef the reference names.
///
/// That is the route through `IlMachineMemberResolution.resolveMemberWithGenerics`, which is the
/// counterpart of the virtual-dispatch route covered by
/// `sourcesPure/FnPtrCallConvOverloadDispatch.cs`. CoreCLR answers it with
/// `MemberLoader::FindMethod` -> `MetaSig::CompareMethodSigs`, which compares the modifier tokens;
/// a comparison of concretised signatures sees one candidate twice.
[<TestFixture>]
module TestCrossAssemblyFnPtrOverload =

    [<Test>]
    let ``a MemberRef picks the overload matching its calling-convention modifiers`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "CrossAssemblyFnPtr.SinkLib"
                        []
                        [
                            """
namespace CrossAssemblyFnPtr.SinkLib;

public static unsafe class Sink
{
    public static int Take (delegate* unmanaged[Cdecl, SuppressGCTransition]<void> f) => 1;

    public static int Take (delegate* unmanaged[Stdcall, SuppressGCTransition]<void> f) => 2;

    // Differing in the CallKind byte alone, which concretisation preserves. These bind correctly
    // even under a modifier-blind comparison, so a failure confined to the pair above is
    // attributable to the modifiers.
    public static int TakeSingle (delegate* unmanaged[Cdecl]<void> f) => 3;

    public static int TakeSingle (delegate* unmanaged[Stdcall]<void> f) => 4;
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "CrossAssemblyFnPtr.SinkEntry"
                        [ "CrossAssemblyFnPtr.SinkLib" ]
                        [
                            """
using CrossAssemblyFnPtr.SinkLib;

unsafe class Program
{
    static int Main(string[] args)
    {
        if (Sink.Take((delegate* unmanaged[Cdecl, SuppressGCTransition]<void>) null) != 1)
        {
            return 1;
        }

        if (Sink.Take((delegate* unmanaged[Stdcall, SuppressGCTransition]<void>) null) != 2)
        {
            return 2;
        }

        if (Sink.TakeSingle((delegate* unmanaged[Cdecl]<void>) null) != 3)
        {
            return 3;
        }

        if (Sink.TakeSingle((delegate* unmanaged[Stdcall]<void>) null) != 4)
        {
            return 4;
        }

        return 7;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "CrossAssemblyFnPtr.SinkEntry"
            // Distinct from every early-return code above, so "the guest ran to the end" and "the
            // guest bailed out at check N" cannot be confused.
            ExpectedReturnCode = 7
        }
        |> CrossAssemblyHarness.runTest
