namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

[<TestFixture>]
module TestNativeEnum =
    type private EnumQCallResult =
        {
            State : IlMachineState
            ValuesArray : ManagedHeapAddress
            NamesArray : ManagedHeapAddress option
        }

    let private enumSource : string =
        """
public enum SignedByte : sbyte
{
    MinusOne = -1,
    Positive = 7
}

public enum UnsignedByte : byte
{
    Max = byte.MaxValue,
    Positive = 7
}

public enum SignedShort : short
{
    MinusOne = -1,
    Min = short.MinValue,
    Positive = 7
}

public enum UnsignedShort : ushort
{
    Max = ushort.MaxValue,
    Positive = 7
}

public enum SignedInt : int
{
    MinusOne = -1,
    Min = int.MinValue,
    Positive = 7
}

public enum SignedLong : long
{
    MinusOne = -1L,
    Min = long.MinValue,
    Positive = 7L
}

public enum UnsignedLong : ulong
{
    Max = ulong.MaxValue,
    Positive = 7UL
}

public enum UnsignedInt : uint
{
    Max = uint.MaxValue,
    Positive = 7u
}

public enum EmptyEnum
{
}

public struct NotEnum
{
    public int Value;
}

public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private prepareEnumProgram
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        : Program.PreparedProgram
        =
        let image =
            Roslyn.compileAssemblyWithResources "NativeEnumTest" OutputKind.ConsoleApplication [] [] [ enumSource ]

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some "NativeEnumTest.cs") peImage dotnetRuntimes Map.empty None [] with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected enum test program to be ready before Main, got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    let private enumGetValuesAndNamesMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let enumType = requiredTopLevelType baseClassTypes.Corelib "System" "Enum"

        let rawMethod =
            enumType.Methods
            |> List.filter (fun method ->
                match method.NativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = "Enum_GetValuesAndNames"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point Enum_GetValuesAndNames not found on System.Enum"
                | methods ->
                    failwith
                        $"QCall entry point Enum_GetValuesAndNames was ambiguous on System.Enum: %d{methods.Length} matches"

        let state, method, _declaringType =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, enumType, method

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
                typeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

        handle, zero, state

    let private qCallTypeHandleValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (enumHandle : ConcreteTypeHandle)
        : CliType * IlMachineState
        =
        let qCallType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "QCallTypeHandle"

        let qCallTypeHandle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state qCallType

        match zero with
        | CliType.ValueType vt ->
            let handleField =
                IlMachineState.requiredOwnInstanceFieldId state qCallTypeHandle "_handle"

            CliValueType.WithFieldSetById
                handleField
                (CliType.Numeric (
                    CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed enumHandle))
                ))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"QCallTypeHandle zero value was not a value type: %O{other}"

    let private objectHandleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : ManagedPointerSource)
        : CliType * IlMachineState
        =
        let objectHandleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let objectHandle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state objectHandleType

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state objectHandle "_ptr"

            CliValueType.WithFieldSetById
                ptrField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer target)))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

    let private allocateObjectOut
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                1
                state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private readObjectOut
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedHeapAddress option
        =
        match IlMachineState.readManagedByref baseClassTypes state ptr with
        | CliType.ObjectRef maybeAddr -> maybeAddr
        | other -> failwith $"expected ObjectHandleOnStack target to contain object ref, got %O{other}"

    let private invokeEnumGetValuesAndNames
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (enumName : string)
        (getNames : bool)
        : EnumQCallResult
        =
        let baseClassTypes = prepared.BaseClassTypes
        let sourceAssembly = state.ActiveAssembly prepared.EntryThread
        let enumType = requiredTopLevelType sourceAssembly "" enumName

        let enumHandle, _zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state enumType

        let qCallTypeHandle, state =
            qCallTypeHandleValue loggerFactory baseClassTypes state enumHandle

        let valuesOut, state = allocateObjectOut baseClassTypes state

        let namesOut, state = allocateObjectOut baseClassTypes state

        let valuesHandle, state =
            objectHandleOnStackValue loggerFactory baseClassTypes state valuesOut

        let namesHandle, state =
            objectHandleOnStackValue loggerFactory baseClassTypes state namesOut

        let state, targetType, qCallMethod =
            enumGetValuesAndNamesMethod loggerFactory baseClassTypes state

        let arguments =
            [
                qCallTypeHandle
                valuesHandle
                namesHandle
                CliType.Numeric (CliNumericType.Int32 (if getNames then 1 else 0))
            ]

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = qCallMethod
                Arguments = ImmutableArray.CreateRange arguments
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = targetType
            }

        let state =
            match NativeQCall.tryExecute ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected Enum_GetValuesAndNames execution result: %O{result}"
            | None -> failwith "Enum_GetValuesAndNames QCall did not match"

        let valuesArray =
            readObjectOut baseClassTypes state valuesOut
            |> Option.defaultWith (fun () -> failwith "Enum_GetValuesAndNames left values out handle null")

        let namesArray = readObjectOut baseClassTypes state namesOut

        {
            State = state
            ValuesArray = valuesArray
            NamesArray = namesArray
        }

    let private assertArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arrayAddr : ManagedHeapAddress)
        (expectedElementType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (expectedElements : CliType list)
        : unit
        =
        let expectedElementHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes expectedElementType

        let array = state.ManagedHeap.Arrays.[arrayAddr]

        array.ConcreteType
        |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero expectedElementHandle)

        array.Elements |> Seq.toList |> shouldEqual expectedElements

    let private assertNames
        (state : IlMachineState)
        (namesArray : ManagedHeapAddress option)
        (expectedNames : string list)
        : unit
        =
        let namesArray =
            namesArray
            |> Option.defaultWith (fun () -> failwith "expected names array to be populated")

        let array = state.ManagedHeap.Arrays.[namesArray]

        let actualNames =
            array.Elements
            |> Seq.map (fun element ->
                match element with
                | CliType.ObjectRef (Some addr) -> state.ManagedHeap.StringContents.[addr]
                | other -> failwith $"expected string object ref in names array, got %O{other}"
            )
            |> Seq.toList

        actualNames |> shouldEqual expectedNames

    [<Test>]
    let ``Enum GetValuesAndNames writes unsigned storage arrays from metadata constants`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareEnumProgram loggerFactory
        let baseClassTypes = prepared.BaseClassTypes

        let signedByte =
            invokeEnumGetValuesAndNames loggerFactory prepared prepared.State "SignedByte" true

        assertArray
            baseClassTypes
            signedByte.State
            signedByte.ValuesArray
            baseClassTypes.Byte
            [
                CliType.Numeric (CliNumericType.UInt8 255uy)
                CliType.Numeric (CliNumericType.UInt8 7uy)
            ]

        assertNames signedByte.State signedByte.NamesArray [ "MinusOne" ; "Positive" ]

        let unsignedByte =
            invokeEnumGetValuesAndNames loggerFactory prepared signedByte.State "UnsignedByte" true

        assertArray
            baseClassTypes
            unsignedByte.State
            unsignedByte.ValuesArray
            baseClassTypes.Byte
            [
                CliType.Numeric (CliNumericType.UInt8 255uy)
                CliType.Numeric (CliNumericType.UInt8 7uy)
            ]

        assertNames unsignedByte.State unsignedByte.NamesArray [ "Max" ; "Positive" ]

        let signedShort =
            invokeEnumGetValuesAndNames loggerFactory prepared unsignedByte.State "SignedShort" true

        assertArray
            baseClassTypes
            signedShort.State
            signedShort.ValuesArray
            baseClassTypes.UInt16
            [
                CliType.Numeric (CliNumericType.UInt16 65535us)
                CliType.Numeric (CliNumericType.UInt16 32768us)
                CliType.Numeric (CliNumericType.UInt16 7us)
            ]

        assertNames signedShort.State signedShort.NamesArray [ "MinusOne" ; "Min" ; "Positive" ]

        let unsignedShort =
            invokeEnumGetValuesAndNames loggerFactory prepared signedShort.State "UnsignedShort" true

        assertArray
            baseClassTypes
            unsignedShort.State
            unsignedShort.ValuesArray
            baseClassTypes.UInt16
            [
                CliType.Numeric (CliNumericType.UInt16 65535us)
                CliType.Numeric (CliNumericType.UInt16 7us)
            ]

        assertNames unsignedShort.State unsignedShort.NamesArray [ "Max" ; "Positive" ]

        let signedInt =
            invokeEnumGetValuesAndNames loggerFactory prepared unsignedShort.State "SignedInt" false

        assertArray
            baseClassTypes
            signedInt.State
            signedInt.ValuesArray
            baseClassTypes.UInt32
            [
                CliType.Numeric (CliNumericType.Int32 -1)
                CliType.Numeric (CliNumericType.Int32 System.Int32.MinValue)
                CliType.Numeric (CliNumericType.Int32 7)
            ]

        signedInt.NamesArray |> shouldEqual None

        let signedLong =
            invokeEnumGetValuesAndNames loggerFactory prepared signedInt.State "SignedLong" true

        assertArray
            baseClassTypes
            signedLong.State
            signedLong.ValuesArray
            baseClassTypes.UInt64
            [
                CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim -1L))
                CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim System.Int64.MinValue))
                CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 7L))
            ]

        assertNames signedLong.State signedLong.NamesArray [ "MinusOne" ; "Min" ; "Positive" ]

        let unsignedLong =
            invokeEnumGetValuesAndNames loggerFactory prepared signedLong.State "UnsignedLong" true

        assertArray
            baseClassTypes
            unsignedLong.State
            unsignedLong.ValuesArray
            baseClassTypes.UInt64
            [
                CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim -1L))
                CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 7L))
            ]

        assertNames unsignedLong.State unsignedLong.NamesArray [ "Max" ; "Positive" ]

        let unsignedInt =
            invokeEnumGetValuesAndNames loggerFactory prepared unsignedLong.State "UnsignedInt" true

        assertArray
            baseClassTypes
            unsignedInt.State
            unsignedInt.ValuesArray
            baseClassTypes.UInt32
            [
                CliType.Numeric (CliNumericType.Int32 -1)
                CliType.Numeric (CliNumericType.Int32 7)
            ]

        assertNames unsignedInt.State unsignedInt.NamesArray [ "Max" ; "Positive" ]

        let emptyEnum =
            invokeEnumGetValuesAndNames loggerFactory prepared unsignedInt.State "EmptyEnum" true

        assertArray baseClassTypes emptyEnum.State emptyEnum.ValuesArray baseClassTypes.UInt32 []
        assertNames emptyEnum.State emptyEnum.NamesArray []

    [<Test>]
    let ``Enum GetValuesAndNames rejects non-enum concrete types`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let prepared = prepareEnumProgram loggerFactory

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeEnumGetValuesAndNames loggerFactory prepared prepared.State "NotEnum" true
                |> ignore
            )

        ex.Message |> shouldContainText "Enum.GetValuesAndNames"
        ex.Message |> shouldContainText "NotEnum"
        ex.Message |> shouldContainText "is not an enum"
