namespace WoofWare.Pawprint.Test

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// `CallSiteTransition` decides whether an entry into a `[UnmanagedCallersOnly]` method is the
/// legal native transition or the fatal managed one, so which side of it a call site falls on is a
/// contract rather than an implementation detail.
///
/// Two axes, and both matter. The *header*'s calling convention (ECMA-335 II.15.3: `DEFAULT` and
/// `VARARG` are the managed ones) is the obvious half. The other is a `modopt` on the return type:
/// `delegate* unmanaged[SuppressGCTransition]<...>` carries the very same `Unmanaged` header as a
/// plain `delegate* unmanaged<...>`, and a classifier reading only the header calls them the same
/// thing — which would let a fatal entry through.
[<TestFixture>]
module TestCallSiteTransition =

    let private assy = typeof<RunResult>.Assembly

    let private signatureReturningVoid (convention : SignatureCallingConvention) : TypeMethodSignature<TypeDefn> =
        {
            Header =
                ComparableSignatureHeader.Make (
                    SignatureHeader (SignatureKind.Method, convention, SignatureAttributes.None)
                )
            ParameterTypes = []
            GenericParameterCount = 0
            RequiredParameterCount = 0
            ReturnType = MethodReturnType.Void
        }

    /// Written out rather than computed, so this says what the mapping *should* be instead of
    /// restating what it is.
    let private expected : (SignatureCallingConvention * IlMachineStateExecution.CallSiteTransition) list =
        [
            SignatureCallingConvention.Default, IlMachineStateExecution.CallSiteTransition.StaysCooperative
            SignatureCallingConvention.VarArgs, IlMachineStateExecution.CallSiteTransition.StaysCooperative
            SignatureCallingConvention.CDecl, IlMachineStateExecution.CallSiteTransition.EntersPreemptive
            SignatureCallingConvention.StdCall, IlMachineStateExecution.CallSiteTransition.EntersPreemptive
            SignatureCallingConvention.ThisCall, IlMachineStateExecution.CallSiteTransition.EntersPreemptive
            SignatureCallingConvention.FastCall, IlMachineStateExecution.CallSiteTransition.EntersPreemptive
            SignatureCallingConvention.Unmanaged, IlMachineStateExecution.CallSiteTransition.EntersPreemptive
        ]

    [<Test>]
    let ``each calling convention lands on the documented side`` () : unit =
        for convention, side in expected do
            IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature (signatureReturningVoid convention)
            |> shouldEqual side

    /// Tied to the enum rather than to the list above: a runtime that grew a new calling convention
    /// would otherwise leave it silently unclassified, and the classifier's `failwith` would first
    /// be discovered by a guest.
    [<Test>]
    let ``the table covers every calling convention the metadata reader can produce`` () : unit =
        let declared =
            Enum.GetValues typeof<SignatureCallingConvention>
            |> Seq.cast<SignatureCallingConvention>
            |> Set.ofSeq

        expected |> List.map fst |> Set.ofList |> shouldEqual declared

    /// Stated on its own so that a change collapsing the two sides together cannot pass by
    /// rewriting the table above in the same edit.
    [<Test>]
    let ``exactly the managed conventions stay cooperative`` () : unit =
        let cooperative =
            Enum.GetValues typeof<SignatureCallingConvention>
            |> Seq.cast<SignatureCallingConvention>
            |> Seq.filter (fun c ->
                IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature (signatureReturningVoid c) = IlMachineStateExecution.CallSiteTransition.StaysCooperative
            )
            |> Set.ofSeq

        cooperative
        |> shouldEqual (Set.ofList [ SignatureCallingConvention.Default ; SignatureCallingConvention.VarArgs ])

    /// Every `calli` call site in a compiled image, decoded exactly as `executeCalli` decodes them.
    let private callSitesOf (source : string) : TypeMethodSignature<TypeDefn> list =
        let image = Roslyn.compile [ source ]

        use stream = new MemoryStream (image)
        use peReader = new PEReader (stream)
        let metadataReader = peReader.GetMetadataReader ()
        let assemblyName = metadataReader.GetAssemblyDefinition().GetAssemblyName ()

        [ 1 .. metadataReader.GetTableRowCount TableIndex.StandAloneSig ]
        |> List.choose (fun row ->
            let standalone =
                metadataReader.GetStandaloneSignature (MetadataTokens.StandaloneSignatureHandle row)

            if standalone.GetKind () = StandaloneSignatureKind.Method then
                standalone.DecodeMethodSignature (TypeDefn.typeProvider assemblyName, ())
                |> TypeMethodSignature.make
                |> Some
            else
                None
        )

    /// A modifier that merely shares the simple name `CallConvSuppressGCTransition` must not be
    /// mistaken for the real one. Fabricated rather than compiled, because C# has no way to emit a
    /// custom modifier naming an arbitrary type — Roslyn only ever writes the `CallConv*` family
    /// from `System.Runtime.CompilerServices` — so no guest can produce this shape and the
    /// real-metadata tests above cannot reach it.
    ///
    /// The direction of the error matters: an unrecognised `modopt` is one the JIT ignores, so real
    /// .NET would perform the transition and admit the call. Matching on the simple name alone
    /// would have PawPrint abort a call the real runtime runs.
    [<Test>]
    let ``a modifier sharing only the simple name is not the suppression`` () : unit =
        let modifierNamed (ns : string) : TypeDefn =
            TypeDefn.FromReference (
                {
                    Handle = ComparableTypeReferenceHandle.Make (MetadataTokens.TypeReferenceHandle 1)
                    Name = "CallConvSuppressGCTransition"
                    Namespace = ns
                    ResolutionScope = TypeRefResolutionScope.ModuleRef (MetadataTokens.ModuleReferenceHandle 1)
                },
                SignatureTypeKind.Class
            )

        let signatureModifiedBy (modifier : TypeDefn) : TypeMethodSignature<TypeDefn> =
            { signatureReturningVoid SignatureCallingConvention.Unmanaged with
                ReturnType =
                    MethodReturnType.Returns (
                        TypeDefn.Modified
                            {
                                Unmodified = TypeDefn.PrimitiveType PrimitiveType.Int32
                                Modifier = modifier
                                IsRequired = false
                            }
                    )
            }

        // The decoy leaves the classification alone...
        signatureModifiedBy (modifierNamed "NotInterop")
        |> IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature
        |> shouldEqual IlMachineStateExecution.CallSiteTransition.EntersPreemptive

        // ...and the genuine article, identical but for its namespace, does not. Both halves are
        // here so that a classifier which simply answered `EntersPreemptive` cannot pass.
        signatureModifiedBy (modifierNamed "System.Runtime.CompilerServices")
        |> IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature
        |> shouldEqual IlMachineStateExecution.CallSiteTransition.StaysCooperative

    /// The modifier axis, read off metadata Roslyn actually emitted rather than off a signature
    /// fabricated to match what this file believes the encoding to be. The guest's two call sites
    /// differ *only* in the suppression, so if the classifier could not see the `modopt` the two
    /// would come back equal and this would fail.
    [<Test>]
    let ``a suppressed GC transition is read off the call site's modopt`` () : unit =
        let source =
            """
using System;
using System.Runtime.InteropServices;

public static unsafe class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x) { return x * 2; }

    public static int Main ()
    {
        delegate* unmanaged<int, int> plain = &Doubler;
        int total = plain (1);

        nint raw = (nint) plain;
        delegate* unmanaged[SuppressGCTransition]<int, int> suppressed =
            (delegate* unmanaged[SuppressGCTransition]<int, int>) raw;
        total += suppressed (2);

        return total;
    }
}
"""

        let callSites = callSitesOf source

        // The guest writes exactly two function-pointer call sites, and both carry the `Unmanaged`
        // header -- which is the whole point: the header cannot tell them apart.
        callSites.Length |> shouldEqual 2

        for callSite in callSites do
            callSite.Header.Get.CallingConvention
            |> shouldEqual SignatureCallingConvention.Unmanaged

        callSites
        |> List.map IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature
        |> List.sort
        |> shouldEqual
            [
                IlMachineStateExecution.CallSiteTransition.EntersPreemptive
                IlMachineStateExecution.CallSiteTransition.StaysCooperative
            ]

    /// A call site may name several conventions at once, and then the suppression is not the only
    /// modifier on the return type. A walk that looked at just the outermost one would answer
    /// `EntersPreemptive` for `unmanaged[Cdecl, SuppressGCTransition]` depending on the order
    /// Roslyn happened to emit them in, which is not a thing to leave to chance.
    ///
    /// `unmanaged[Cdecl]` alongside it is the control: a modifier that is *not* the suppression
    /// must leave the classification alone.
    [<Test>]
    let ``the suppression is found among several convention modifiers`` () : unit =
        let source =
            """
using System;
using System.Runtime.InteropServices;

public static unsafe class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x) { return x * 2; }

    public static int Main ()
    {
        delegate* unmanaged<int, int> plain = &Doubler;
        nint raw = (nint) plain;

        delegate* unmanaged[Cdecl]<int, int> cdecl = (delegate* unmanaged[Cdecl]<int, int>) raw;
        delegate* unmanaged[Cdecl, SuppressGCTransition]<int, int> both =
            (delegate* unmanaged[Cdecl, SuppressGCTransition]<int, int>) raw;

        return plain (1) + cdecl (2) + both (3);
    }
}
"""

        callSitesOf source
        |> List.map IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature
        |> List.sort
        |> shouldEqual
            [
                IlMachineStateExecution.CallSiteTransition.EntersPreemptive
                IlMachineStateExecution.CallSiteTransition.EntersPreemptive
                IlMachineStateExecution.CallSiteTransition.StaysCooperative
            ]
