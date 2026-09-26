namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <see cref="InternalSignatureTypeResolution" />: what the handle behind an
/// <c>ELEMENT_TYPE_INTERNAL</c> run is, as <c>DynamicSignatureDecoding</c> needs it.
/// </summary>
/// <remarks>
/// The kind is checked directly because nothing downstream of a dynamic method's signature
/// observes it today: concretisation goes by identity. It is still part of the decoded
/// <c>TypeDefn</c>'s identity, so a wrong kind would make a dynamic signature unequal to the same
/// type read from metadata.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInternalSignatureTypeResolution =

    let private guestSource =
        """
public struct Struct { public int X; }
public class Class { }
public enum ByteEnum : byte { A = 1 }
public class GenericClass<T, U> { }
public struct GenericStruct<T> { public T Value; }

public static class Program
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private prepare () : Program.PreparedProgram * DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = Roslyn.compile [ guestSource ]

        let prepared =
            use peImage = new MemoryStream (image)

            match
                Program.prepare
                    loggerFactory
                    (Some "InternalSignatureTypeResolutionGuest.cs")
                    peImage
                    (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
            with
            | Program.ProgramStartResult.Ready prepared -> prepared
            | Program.ProgramStartResult.CompletedBeforeMain outcome ->
                failwith $"expected guest to be ready before Main, but got %O{outcome}"

        let guest =
            prepared.State.LoadedAssembly prepared.State.EntryAssembly.FullName
            |> Option.defaultWith (fun () -> failwith "the guest assembly is not loaded")

        prepared, guest

    let private typeNamed (assembly : DumpedAssembly) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        assembly.TryGetTopLevelTypeDef "" name
        |> Option.defaultWith (fun () -> failwith $"type %s{name} not found in the guest")

    let private concretise
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (typeDefn : TypeDefn)
        : IlMachineState * ConcreteTypeHandle
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        IlMachineState.concretizeType
            loggerFactory
            prepared.BaseClassTypes
            state
            prepared.BaseClassTypes.Corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            typeDefn

    let private resolve
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : InternalSignatureType
        =
        InternalSignatureTypeResolution.ofHandle
            "test"
            prepared.BaseClassTypes
            state
            (NativeIntSource.TypeHandlePtr target)

    /// A closed non-generic type is `NonGeneric`, with the kind its base chain decides. The kind
    /// given to concretisation is deliberately `Class` throughout, so an answer that echoed the
    /// handle's own construction rather than reading the type would be caught on the value types.
    [<Test>]
    let ``a closed non-generic type resolves to its identity and kind`` () : unit =
        let prepared, guest = prepare ()
        let bct = prepared.BaseClassTypes

        let cases =
            [
                typeNamed guest "Struct", SignatureTypeKind.ValueType
                typeNamed guest "Class", SignatureTypeKind.Class
                typeNamed guest "ByteEnum", SignatureTypeKind.ValueType
                bct.Int32, SignatureTypeKind.ValueType
                bct.Object, SignatureTypeKind.Class
                // `System.Enum` and `System.ValueType` are themselves classes, though everything
                // deriving from them is a value type.
                bct.Enum, SignatureTypeKind.Class
                bct.ValueType, SignatureTypeKind.Class
            ]

        let mutable state = prepared.State

        for typeInfo, expectedKind in cases do
            let newState, handle =
                concretise prepared state (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.Class))

            state <- newState

            resolve prepared state (RuntimeTypeHandleTarget.Closed handle)
            |> shouldEqual (InternalSignatureType.NonGeneric (typeInfo.Identity, expectedKind))

    [<Test>]
    let ``an open generic definition resolves to its identity, kind and arity`` () : unit =
        let prepared, guest = prepare ()

        for name, expectedKind, arity in
            [
                "GenericClass`2", SignatureTypeKind.Class, 2
                "GenericStruct`1", SignatureTypeKind.ValueType, 1
            ] do
            let typeInfo = typeNamed guest name

            resolve prepared prepared.State (RuntimeTypeHandleTarget.OpenGenericTypeDefinition typeInfo.Identity)
            |> shouldEqual (InternalSignatureType.GenericDefinition (typeInfo.Identity, expectedKind, arity))

    /// `SignatureHelper` spells a constructed generic as `GENERICINST` over its definition, so a
    /// run naming one is not its output.
    [<Test>]
    let ``a constructed generic is refused`` () : unit =
        let prepared, guest = prepare ()
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32

        let state, handle =
            concretise
                prepared
                prepared.State
                (TypeDefn.GenericInstantiation (
                    TypeDefn.FromDefinition ((typeNamed guest "GenericStruct`1").Identity, SignatureTypeKind.ValueType),
                    ImmutableArray.Create int32
                ))

        let exn =
            Assert.Throws<Exception> (fun () ->
                resolve prepared state (RuntimeTypeHandleTarget.Closed handle) |> ignore
            )

        exn.Message |> shouldContainText "constructed generic"

    /// Structural types are spelled structurally, so a run naming one is not `SignatureHelper`'s
    /// output either.
    [<Test>]
    let ``a structural type or a generic parameter is refused`` () : unit =
        let prepared, guest = prepare ()

        let state, handle =
            concretise
                prepared
                prepared.State
                (TypeDefn.FromDefinition ((typeNamed guest "Struct").Identity, SignatureTypeKind.ValueType))

        for target in
            [
                RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero handle)
                RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref handle)
                RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer handle)
                RuntimeTypeHandleTarget.GenericParameter ((typeNamed guest "GenericClass`2").Identity, 0)
            ] do
            let exn =
                Assert.Throws<Exception> (fun () -> resolve prepared state target |> ignore)

            exn.Message |> shouldContainText "structurally"

    [<Test>]
    let ``a native int that is not a type handle is refused`` () : unit =
        let prepared, _ = prepare ()

        let exn =
            Assert.Throws<Exception> (fun () ->
                InternalSignatureTypeResolution.ofHandle
                    "test"
                    prepared.BaseClassTypes
                    prepared.State
                    (NativeIntSource.Verbatim 5L)
                |> ignore
            )

        exn.Message |> shouldContainText "not a type handle"
