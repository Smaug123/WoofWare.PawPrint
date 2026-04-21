namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestPrimitiveLikeStructRegistry =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private mkCt (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteType<int> =
        ConcreteType.makeFromIdentity ti.Identity ti.Namespace ti.Name ImmutableArray<int>.Empty

    let private mkCtWithGenerics
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (generics : int ImmutableArray)
        : ConcreteType<int>
        =
        ConcreteType.makeFromIdentity ti.Identity ti.Namespace ti.Name generics

    [<Test>]
    let ``IntPtr flattens to native int`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.IntPtr)
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToNativeInt)

    [<Test>]
    let ``UIntPtr flattens to native int`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.UIntPtr)
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToNativeInt)

    [<Test>]
    let ``RuntimeTypeHandle flattens to object ref`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.RuntimeTypeHandle)
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToObjectRef)

    [<Test>]
    let ``RuntimeMethodHandle flattens to object ref`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.RuntimeMethodHandle)
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToObjectRef)

    [<Test>]
    let ``RuntimeFieldHandle flattens to object ref`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.RuntimeFieldHandle)
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToObjectRef)

    [<Test>]
    let ``RuntimeFieldHandleInternal flattens to runtime pointer`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.RuntimeFieldHandleInternal)
        |> shouldEqual (Some PrimitiveLikeKind.FlattenToRuntimePointer)

    [<Test>]
    let ``ByReference (if present on this corelib) flattens to managed pointer`` () : unit =
        match bct.ByReference with
        | None -> Assert.Inconclusive "corelib under test does not expose System.ByReference"
        | Some br ->
            PrimitiveLikeStruct.kind bct (mkCt br)
            |> shouldEqual (Some PrimitiveLikeKind.FlattenToManagedPointer)

    [<Test>]
    let ``ByReference matches on definition identity, ignoring generic instantiation`` () : unit =
        match bct.ByReference with
        | None -> Assert.Inconclusive "corelib under test does not expose System.ByReference"
        | Some br ->
            let generics = ImmutableArray.Create<int> (42)

            PrimitiveLikeStruct.kind bct (mkCtWithGenerics br generics)
            |> shouldEqual (Some PrimitiveLikeKind.FlattenToManagedPointer)

    [<Test>]
    let ``String is not primitive-like`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.String) |> shouldEqual None

    [<Test>]
    let ``Int32 is not primitive-like`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.Int32) |> shouldEqual None

    [<Test>]
    let ``Object is not primitive-like`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.Object) |> shouldEqual None

    [<Test>]
    let ``Exception is not primitive-like`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.Exception) |> shouldEqual None

    [<Test>]
    let ``RuntimeType is not primitive-like`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.RuntimeType) |> shouldEqual None

    [<Test>]
    let ``TypedReference is not primitive-like`` () : unit =
        PrimitiveLikeStruct.kind bct (mkCt bct.TypedReference) |> shouldEqual None

    [<Test>]
    let ``isPrimitiveLike agrees with kind`` () : unit =
        let candidates : TypeInfo<GenericParamFromMetadata, TypeDefn> list =
            [
                bct.IntPtr
                bct.UIntPtr
                bct.RuntimeTypeHandle
                bct.RuntimeMethodHandle
                bct.RuntimeFieldHandle
                bct.RuntimeFieldHandleInternal
                bct.String
                bct.Int32
                bct.Object
                bct.Exception
                bct.RuntimeType
                bct.TypedReference
            ]

        for ti in candidates do
            let ct = mkCt ti

            PrimitiveLikeStruct.isPrimitiveLike bct ct
            |> shouldEqual (PrimitiveLikeStruct.kind bct ct |> Option.isSome)
