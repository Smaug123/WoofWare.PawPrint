namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Tests for the `AssemblyNative_GetSimpleName` QCall, which CoreCLR answers from the
/// `Name` column of the manifest's single `Assembly` metadata row.
///
/// There is no end-to-end guest coverage yet: the only managed caller a guest can reach
/// is `Assembly.GetName()`, which goes on to need `AssemblyNative_GetVersion`,
/// `_GetLocale`, `_GetPublicKey`, `_GetFlags` and `_GetCodeBase` as well. The parked
/// `sourcesPure/AssemblyGetNameSimpleName.cs` records that; these tests pin the QCall
/// itself in the meantime.
[<TestFixture>]
module TestAssemblyNativeSimpleName =

    /// Deliberately dotted. CoreCLR reads the simple name straight out of metadata, so a
    /// dotted name comes back whole; an implementation that split a qualified name at a
    /// '.' (as the type-name QCalls legitimately do) would truncate it.
    let private guestAssemblyName = "WoofWare.PawPrint.SimpleNameTestGuest"

    let private guestSource =
        """
public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    /// The oracle: the `Name` column of the `Assembly` row, read from the same image with
    /// `MetadataReader` — i.e. exactly what CoreCLR's `GetAssemblyProps(TokenFromRid(1,
    /// mdtAssembly), ...)` hands back. Reading it from the image rather than restating the
    /// constant keeps the test honest if the compiler ever mangles the name it was given.
    let private metadataAssemblyName (image : byte[]) : string =
        use peImage = new MemoryStream (image)
        use peReader = new System.Reflection.PortableExecutable.PEReader (peImage)
        let metadata = peReader.GetMetadataReader ()
        metadata.GetString (metadata.GetAssemblyDefinition().Name)

    let private prepareGuest
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (image : byte[])
        : Program.PreparedProgram
        =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "SimpleNameTestGuest.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected guest to be ready before Main, but got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () ->
            failwith $"type %s{namespaceName}.%s{typeName} not found in %s{assembly.Name.Name}"
        )

    let private getSimpleNameMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let runtimeAssemblyType =
            requiredTopLevelType baseClassTypes.Corelib "System.Reflection" "RuntimeAssembly"

        let rawMethod =
            runtimeAssemblyType.Methods
            |> List.filter (fun method ->
                match method.NativeImport with
                | Some import ->
                    import.ModuleName = "QCall"
                    && import.EntryPointName = "AssemblyNative_GetSimpleName"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point AssemblyNative_GetSimpleName not found on RuntimeAssembly"
                | methods ->
                    failwith
                        $"QCall entry point AssemblyNative_GetSimpleName was ambiguous on RuntimeAssembly: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, runtimeAssemblyType, method

    let private concreteValueTypeZero
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ConcreteTypeHandle * CliType * IlMachineState
        =
        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle
        handle, zero, state

    /// `struct QCallAssembly { void* _ptr; IntPtr _assembly; }`, with `_assembly` carrying
    /// the tag that PawPrint uses in place of CoreCLR's native `Assembly*`.
    let private qCallAssemblyValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyFullName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let qCallAssemblyType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "QCallAssembly"

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state qCallAssemblyType

        match zero with
        | CliType.ValueType vt ->
            let assemblyField =
                IlMachineState.requiredOwnInstanceFieldId state handle "_assembly"

            CliValueType.WithFieldSetById
                assemblyField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"QCallAssembly zero value was not a value type: %O{other}"

    /// Mirrors the `new StringHandleOnStack(ref name)` the C# wrapper builds over a local
    /// preinitialised to null; the object[1] cell stands in for that stack slot.
    let private stringHandleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                1
                state

        let target = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let stringHandleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "StringHandleOnStack"

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state stringHandleType

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            let value =
                CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
                |> CliType.ValueType

            value, target, state
        | other -> failwith $"StringHandleOnStack zero value was not a value type: %O{other}"

    /// Runs the QCall for `assemblyFullName` and returns the heap address the handler wrote
    /// into the `StringHandleOnStack` (None if it left the slot at its preinitialised null).
    let private invokeGetSimpleName
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * ManagedHeapAddress option
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        let stringHandle, target, state =
            stringHandleOnStackValue loggerFactory baseClassTypes state

        let state, runtimeAssemblyType, qCallMethod =
            getSimpleNameMethod loggerFactory baseClassTypes state

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = qCallMethod
                Arguments = ImmutableArray.CreateRange [ qCallAssembly ; stringHandle ]
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = runtimeAssemblyType
            }

        let state =
            match NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetSimpleName" ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected AssemblyNative_GetSimpleName execution result: %O{result}"
            | None -> failwith "AssemblyNative_GetSimpleName QCall did not match"

        let written =
            match IlMachineState.readManagedByref baseClassTypes state target with
            | CliType.ObjectRef maybeAddr -> maybeAddr
            | other -> failwith $"expected StringHandleOnStack target to contain an object ref, got %O{other}"

        state, written

    /// Asserts that `addr` is a genuine `System.String` heap object carrying `expected`,
    /// rather than merely a side-table entry.
    let private assertIsString
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (expected : string)
        : unit
        =
        ManagedHeap.getStringContents addr state.ManagedHeap
        |> shouldEqual (Some expected)

        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let stringHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.String

        heapObj.ConcreteType |> shouldEqual stringHandle

        let lengthField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "_stringLength"

        AllocatedNonArrayObject.DereferenceFieldById lengthField heapObj
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 expected.Length))

    [<Test>]
    let ``GetSimpleName returns the Assembly metadata row's name`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        // Sanity: the oracle is reading the name we asked for, so the assertions below
        // are not vacuously comparing two copies of the same mistake.
        metadataAssemblyName image |> shouldEqual guestAssemblyName

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        // The handle the QCall is keyed by is the *full* name, which carries version,
        // culture and public key token on top of the simple name. The point of the test
        // is that the handler answers with the metadata field rather than a prefix of
        // this string.
        guest.Name.FullName |> shouldNotEqual guestAssemblyName

        guest.Name.FullName.StartsWith (guestAssemblyName + ", ", System.StringComparison.Ordinal)
        |> shouldEqual true

        let state, written =
            invokeGetSimpleName loggerFactory prepared prepared.State guest.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state addr (metadataAssemblyName image)

    [<Test>]
    let ``GetSimpleName answers for corelib too`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes

        let state, written =
            invokeGetSimpleName loggerFactory prepared prepared.State baseClassTypes.Corelib.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state addr "System.Private.CoreLib"

    [<Test>]
    let ``GetSimpleName allocates a fresh string per call`` () : unit =
        // CoreCLR's `StringHandleOnStack::Set(LPCUTF8)` goes through
        // `StringObject::NewString`, which interns nothing above zero length, so
        // `ReferenceEquals` across two calls is false there. Guest code that cached the
        // result by reference would be relying on behaviour the real runtime does not
        // provide, so PawPrint must not accidentally supply it.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, first =
            invokeGetSimpleName loggerFactory prepared prepared.State guest.Name.FullName

        let _state, second =
            invokeGetSimpleName loggerFactory prepared state guest.Name.FullName

        first |> shouldNotEqual None
        second |> shouldNotEqual None
        first |> shouldNotEqual second

    [<Test>]
    let ``GetSimpleName on an unloaded assembly fails loudly`` () : unit =
        // The handle decodes to an assembly identity we have never loaded, which means a
        // caller invented one. Answering anything at all would be a guess.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSimpleName
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * ManagedHeapAddress option>
            )

        exn.Message |> shouldContainText "is not loaded"
