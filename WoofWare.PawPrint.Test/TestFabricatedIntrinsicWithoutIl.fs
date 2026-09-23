namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An `[Intrinsic]` method with no IL body runs the implementation its body kind names.
///
/// PawPrint refuses to *interpret* an `[Intrinsic]` method's IL unless that IL has been reviewed,
/// because the shipped body can be a placeholder the JIT always replaces. A method with no IL has
/// nothing to misinterpret: a P/Invoke, an InternalCall or a runtime-synthesised body is already
/// PawPrint's own implementation, so the marker must not stop the call reaching it. .NET 11 put
/// `[Intrinsic]` on the `String.FastAllocateString(MethodTable*, nint)` InternalCall, which is how
/// this was found.
///
/// No .NET 10 framework assembly has a native-bodied `[Intrinsic]` that PawPrint implements
/// natively rather than in `Intrinsics.call` (all 46 are `Math`/`MathF` InternalCalls), so the shape
/// is fabricated. CoreCLR honours `[Intrinsic]` only in CoreLib (`methodtablebuilder.cpp`, the
/// `GetModule()->IsSystem()` guards), so the real runtime just runs these methods; PawPrint
/// classifies an application through a MemberRef by the attribute's name alone, so it treats them as
/// intrinsics. That difference is what lets a non-CoreLib image reach the gate, and the fixture
/// checks PawPrint still classifies each method as `[Intrinsic]` so it cannot pass vacuously once
/// the difference is gone.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedIntrinsicWithoutIl =

    let private fabricatedName = "IntrinsicWithoutIl"

    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName fabricatedName, typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule fabricatedName

        // CoreLib's own `internal sealed class IntrinsicAttribute`; the image references it through
        // a MemberRef on a TypeRef scoped to System.Private.CoreLib.
        let intrinsic =
            let ty =
                typeof<obj>.Assembly.GetType ("System.Runtime.CompilerServices.IntrinsicAttribute", true)

            let ctor =
                ty.GetConstructor (
                    BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic,
                    Type.EmptyTypes
                )

            CustomAttributeBuilder (ctor, Array.empty)

        let natives =
            modul.DefineType ("Natives", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        // `int32_t SystemNative_GetMaximumAddressSize(void)`, as
        // `sourcesPure/SystemNativeGetMaximumAddressSize.cs` declares it, which PawPrint answers in
        // `NativeSystemNative` and which the real shim answers on every Unix.
        let maximumAddressSize =
            natives.DefinePInvokeMethod (
                "MaximumAddressSize",
                "libSystem.Native",
                "SystemNative_GetMaximumAddressSize",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.PinvokeImpl,
                CallingConventions.Standard,
                typeof<int>,
                Type.EmptyTypes,
                CallingConvention.Winapi,
                CharSet.Ansi
            )

        maximumAddressSize.SetImplementationFlags MethodImplAttributes.PreserveSig
        maximumAddressSize.SetCustomAttribute intrinsic
        natives.CreateType () |> ignore<Type>

        // An InternalCall no handler serves. Its own type, so that whatever CoreCLR makes of an
        // InternalCall outside CoreLib cannot reach `Natives`.
        let internalCalls =
            modul.DefineType (
                "InternalCalls",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let unserved =
            internalCalls.DefineMethod (
                "Unserved",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        unserved.SetImplementationFlags MethodImplAttributes.InternalCall
        unserved.SetCustomAttribute intrinsic
        internalCalls.CreateType () |> ignore<Type>

        // `delegate int IntrinsicInvoke()`, whose runtime-synthesised `Invoke` carries the marker.
        let del =
            modul.DefineType (
                "IntrinsicInvoke",
                TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.AutoClass,
                typeof<MulticastDelegate>
            )

        let ctor =
            del.DefineConstructor (
                MethodAttributes.Public
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName,
                CallingConventions.Standard,
                [| typeof<obj> ; typeof<nativeint> |]
            )

        ctor.SetImplementationFlags (MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed)

        let invoke =
            del.DefineMethod (
                "Invoke",
                MethodAttributes.Public
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.Virtual,
                typeof<int>,
                Type.EmptyTypes
            )

        invoke.SetImplementationFlags (MethodImplAttributes.Runtime ||| MethodImplAttributes.Managed)
        invoke.SetCustomAttribute intrinsic
        del.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// The fabricated method `typeName::methodName` as PawPrint reads it, and whether PawPrint
    /// classifies it as `[Intrinsic]`.
    let private readAndClassify
        (image : byte[])
        (typeName : string)
        (methodName : string)
        : MethodBody<TypeDefn> * bool
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use stream = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None stream

        let getMemberRefParentType (handle : MemberReferenceHandle) : TypeRef =
            match assembly.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> assembly.TypeRefs.[r]
            | other -> failwith $"fabricated MemberRef has unexpected parent %O{other}"

        let method =
            assembly.Methods.Values
            |> Seq.filter (fun m -> m.Name = methodName && m.RequiredDeclaringType.Name = typeName)
            |> Seq.exactlyOne

        method.Body, MethodInfo.isJITIntrinsic getMemberRefParentType assembly.Methods method

    [<Test>]
    let ``the fabricated methods are native-bodied intrinsics as PawPrint reads them`` () : unit =
        let image = fabricate ()

        match readAndClassify image "Natives" "MaximumAddressSize" with
        | MethodBody.PInvoke, isIntrinsic -> isIntrinsic |> shouldEqual true
        | body, _ -> failwith $"expected a P/Invoke body, got %O{body}"

        match readAndClassify image "InternalCalls" "Unserved" with
        | MethodBody.InternalCall, isIntrinsic -> isIntrinsic |> shouldEqual true
        | body, _ -> failwith $"expected an InternalCall body, got %O{body}"

        match readAndClassify image "IntrinsicInvoke" "Invoke" with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke, isIntrinsic -> isIntrinsic |> shouldEqual true
        | body, _ -> failwith $"expected a runtime-provided delegate Invoke body, got %O{body}"

    [<Test>]
    let ``an Intrinsic P/Invoke reaches native dispatch`` () : unit =
        // sizeof(struct sockaddr_storage) on both Unix families; see
        // `sourcesPure/SystemNativeGetMaximumAddressSize.cs` for why it is a cross-runtime fact.
        let driver =
            """
public static class Driver
{
    public static int Main()
    {
        return Natives.MaximumAddressSize() == 128 ? 0 : 1;
    }
}
"""

        FabricatedGuest.run fabricatedName (fabricate ()) "IntrinsicPInvokeDriver" driver 0

    [<Test>]
    let ``an Intrinsic runtime-provided delegate Invoke reaches delegate dispatch`` () : unit =
        let driver =
            """
public static class Driver
{
    public static int Main()
    {
        IntrinsicInvoke answer = () => 42;
        return answer() == 42 ? 0 : 1;
    }
}
"""

        FabricatedGuest.run fabricatedName (fabricate ()) "IntrinsicDelegateDriver" driver 0

    [<Test>]
    let ``an Intrinsic InternalCall reaches native dispatch`` () : unit =
        // CoreCLR serves an InternalCall only from CoreLib, and raises SecurityException for any
        // other (`ecall.cpp`, `BFA_ECALLS_MUST_BE_IN_SYS_MOD`); no handler in `NativeDispatch`
        // serves one either. So this cannot compare a result, only where the call went: PawPrint
        // must stop in native dispatch, naming the method, rather than at the `[Intrinsic]` check.
        let driver =
            """
using System.Runtime.CompilerServices;
using System.Security;

public static class Driver
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Call() => InternalCalls.Unserved();

    public static int Main()
    {
        try
        {
            Call();
            return 1;
        }
        catch (SecurityException)
        {
            return 0;
        }
    }
}
"""

        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth fabricatedName (fabricate ()) "IntrinsicInternalCallDriver" driver

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)

        match onPawPrint with
        | FabricatedOutcome.Exited code ->
            failwith $"expected PawPrint to refuse the unserved InternalCall, but the guest exited %d{code}"
        | FabricatedOutcome.Failed e ->
            let rec innermost (e : exn) : exn =
                match e.InnerException with
                | null -> e
                | inner -> innermost inner

            (innermost e).Message
            |> shouldContainText
                "Unimplemented native method (InternalCall): IntrinsicWithoutIl .InternalCalls::Unserved()"
