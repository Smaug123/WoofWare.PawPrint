namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Reflection.Metadata
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// What PawPrint answers for a field of CoreLib's `TypeDesc` struct, read through the `TypeDesc*`
/// of a type CoreCLR represents as a `TypeDesc` (a type variable, a byref, a pointer), against the
/// word the test host's own CoreCLR keeps for the same type.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTypeDescProjection =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory HostImage.coreLibPath

    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) bct AllConcreteTypes.Empty

    let private topLevelType (``namespace`` : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef ``namespace`` name with
        | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
        | Some typeInfo -> typeInfo

    let private typeAndFlagsField : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        (topLevelType "System.Runtime.CompilerServices" "TypeDesc").Fields
        |> List.filter (fun field -> field.Name = "_typeAndFlags")
        |> List.exactlyOne

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Int32

    let private listT : RuntimeTypeHandleTarget =
        RuntimeTypeHandleTarget.GenericParameter ((topLevelType "System.Collections.Generic" "List`1").Identity, 0)

    let private arrayEmptyT : RuntimeTypeHandleTarget =
        let array = topLevelType "System" "Array"

        let empty =
            array.Methods
            |> List.filter (fun m -> m.Name = "Empty" && m.Signature.GenericParameterCount = 1)
            |> List.exactlyOne

        RuntimeTypeHandleTarget.MethodGenericParameter (
            array.Identity,
            ComparableMethodDefinitionHandle.Make empty.TryMetadata.Value.Handle,
            0
        )

    let private hostListT : Type = typedefof<List<obj>>.GetGenericArguments().[0]

    let private hostArrayEmptyT : Type =
        typeof<Array>.GetMethod("Empty").GetGenericArguments().[0]

    /// Each `TypeDesc`-shaped type, as PawPrint names it and as the host's reflection does.
    let private shapes : Map<string, RuntimeTypeHandleTarget * Type> =
        [
            "type variable", (listT, hostListT)
            "method type variable", (arrayEmptyT, hostArrayEmptyT)
            "byref",
            (RuntimeTypeHandleTarget.composite CompositeShape.Byref (RuntimeTypeHandleTarget.Closed int32Handle),
             typeof<int>.MakeByRefType ())
            "pointer",
            (RuntimeTypeHandleTarget.composite CompositeShape.Pointer (RuntimeTypeHandleTarget.Closed int32Handle),
             typeof<int>.MakePointerType ())
            "pointer to pointer",
            (RuntimeTypeHandleTarget.composite
                CompositeShape.Pointer
                (RuntimeTypeHandleTarget.composite CompositeShape.Pointer (RuntimeTypeHandleTarget.Closed int32Handle)),
             typeof<int>.MakePointerType().MakePointerType ())
            "byref of a type variable",
            (RuntimeTypeHandleTarget.composite CompositeShape.Byref listT, hostListT.MakeByRefType ())
            "pointer to a method type variable",
            (RuntimeTypeHandleTarget.composite CompositeShape.Pointer arrayEmptyT, hostArrayEmptyT.MakePointerType ())
        ]
        |> Map.ofList

    let shapeNames : string list = shapes |> Map.keys |> List.ofSeq

    /// The host CoreCLR's `TypeDesc::_typeAndFlags` for `t`: the first word of the `TypeDesc` its
    /// type handle names, once the handle's TypeDesc tag (bit 1) is cleared.
    let private hostTypeAndFlags (t : Type) : uint32 =
        let handle = t.TypeHandle.Value

        if handle &&& 2n = 0n then
            failwith $"%O{t} is not a TypeDesc on the host: its type handle %x{handle} has no TypeDesc tag"

        uint32 (Marshal.ReadInt32 (handle &&& ~~~2n))

    [<TestCaseSource(nameof shapeNames)>]
    let ``TypeDesc::_typeAndFlags is the host CoreCLR's own word`` (shape : string) : unit =
        let target, hostType = shapes.[shape]

        let projected =
            match MethodTableProjection.tryProjectTypeDescField bct typeAndFlagsField target with
            | Some (CliType.Numeric (CliNumericType.Int32 word)) -> uint32 word
            | other -> failwith $"expected TypeDesc::_typeAndFlags to project as a uint32, got %O{other}"

        projected |> shouldEqual (hostTypeAndFlags hostType)

    [<Test>]
    let ``the shapes are each of the TypeDesc kinds`` () : unit =
        // ELEMENT_TYPE_VAR, MVAR, BYREF and PTR: every TypeDesc a guest can name but a function
        // pointer, whose host `Type` F# cannot spell.
        shapes
        |> Map.values
        |> Seq.map (fun (_, t) -> hostTypeAndFlags t &&& 0xFFu)
        |> Set.ofSeq
        |> shouldEqual (Set.ofList [ 0x13u ; 0x1Eu ; 0x10u ; 0x0Fu ])
