namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

/// `DelegateLayout.classify` recognises exactly the `Delegate`/`MulticastDelegate` layouts
/// PawPrint knows how to write and read, and names the fields of any other; and every writer and
/// reader of a delegate's fields goes through that classification.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDelegateLayout =

    /// A candidate layout: the instance fields of the `Delegate` stand-in and of the
    /// `MulticastDelegate` stand-in, as C# (type, name) pairs in declaration order.
    type private Variant = (string * string) list * (string * string) list

    let private net10 : Variant =
        [
            "object", "_target"
            "object", "_methodBase"
            "IntPtr", "_methodPtr"
            "IntPtr", "_methodPtrAux"
        ],
        [ "object", "_invocationList" ; "IntPtr", "_invocationCount" ]

    /// As .NET 11 (preview 7 and RC1) declares them: `MulticastDelegate` has no fields, and the
    /// invocation list and count live in `Delegate`'s `_helperObject` and `_extraData`.
    let private net11 : Variant =
        [
            "object", "_helperObject"
            "object", "_target"
            "IntPtr", "_methodPtr"
            "IntPtr", "_methodPtrAux"
            "IntPtr", "_extraData"
        ],
        []

    let private typeAlphabet : string list =
        [ "object" ; "IntPtr" ; "UIntPtr" ; "int" ; "long" ; "string" ]

    let private replaceAt (i : int) (x : 'a) (l : 'a list) : 'a list =
        l |> List.mapi (fun j y -> if i = j then x else y)

    let private removeAt (i : int) (l : 'a list) : 'a list =
        l |> List.indexed |> List.filter (fun (j, _) -> j <> i) |> List.map snd

    let private insertAt (i : int) (x : 'a) (l : 'a list) : 'a list = List.take i l @ [ x ] @ List.skip i l

    /// Every field list one edit away from `fields`: a field removed, renamed, retyped, swapped
    /// with its neighbour, or an extra field inserted.
    let private oneEditFrom (fields : (string * string) list) : (string * string) list list =
        let n = fields.Length

        [
            for i in 0 .. n - 1 do
                yield removeAt i fields

                let ty, name = fields.[i]
                yield replaceAt i (ty, name + "_") fields

                for other in typeAlphabet do
                    if other <> ty then
                        yield replaceAt i (other, name) fields

            for i in 0 .. n - 2 do
                yield replaceAt i fields.[i + 1] (replaceAt (i + 1) fields.[i] fields)

            for i in 0..n do
                yield insertAt i ("object", "_extra") fields
        ]

    /// net10's layout, every layout one edit away from it on either side, the two layouts with a
    /// field moved across the `Delegate`/`MulticastDelegate` boundary; and net11's, every layout
    /// one edit away from its `Delegate`, and net11's with a field on `MulticastDelegate`.
    let private variants : Variant list =
        let delegateFields, multicastFields = net10
        let net11Delegate, _ = net11

        [
            yield net10
            for edited in oneEditFrom delegateFields do
                yield edited, multicastFields
            for edited in oneEditFrom multicastFields do
                yield delegateFields, edited
            yield List.take 3 delegateFields, List.last delegateFields :: multicastFields
            yield delegateFields @ [ List.head multicastFields ], List.tail multicastFields
            yield net11
            for edited in oneEditFrom net11Delegate do
                yield edited, []
            yield net11Delegate, [ "object", "_extra" ]
        ]
        |> List.distinct

    /// The kind and body of the `Wrapper` nested in every fabricated `Delegate{i}`, as net11's
    /// is, with `{D}` standing for the enclosing type's name.
    let private net11Wrapper : string * string = "struct", "internal {D} Value;"

    /// Pairs carrying net11's fields whose nested `Wrapper` is not net11's, by name, with the
    /// kind and body of their `Wrapper`, or `None` for no `Wrapper` at all.
    let private wrapperNearMisses : (string * (string * string) option) list =
        [
            "NoWrapper", None
            "WrapperOfObject", Some ("struct", "internal object Value;")
            "WrapperRenamed", Some ("struct", "internal {D} Other;")
            "WrapperWide", Some ("struct", "internal {D} Value; internal int Extra;")
            "WrapperOfOtherDelegate", Some ("struct", "internal Delegate0 Value;")
            "WrapperClass", Some ("class", "internal {D} Value;")
        ]

    let private classBody (fields : (string * string) list) : string =
        fields
        |> List.map (fun (ty, field) -> $"        private %s{ty} %s{field};")
        |> String.concat "\n"

    /// A `Delegate{i}`, with a nested `Wrapper`, and a `Multicast{i} : Delegate{i}` per variant;
    /// a pair carrying net10's fields together with statics; and the `wrapperNearMisses`.
    let private fabricated : Lazy<DumpedAssembly> =
        lazy
            let pairWith
                (wrapper : (string * string) option)
                (name : string)
                ((delegateFields, multicastFields) : Variant)
                =
                let wrapper =
                    match wrapper with
                    | None -> ""
                    | Some (kind, body) ->
                        let body = body.Replace ("{D}", $"Delegate%s{name}")
                        $"\n        internal %s{kind} Wrapper {{ %s{body} }}"

                $"    internal abstract class Delegate%s{name}\n    {{\n%s{classBody delegateFields}%s{wrapper}\n    }}\n"
                + $"    internal abstract class Multicast%s{name} : Delegate%s{name}\n    {{\n%s{classBody multicastFields}\n    }}"

            let pair (name : string) (variant : Variant) : string =
                pairWith (Some net11Wrapper) name variant

            let withStatics =
                (pair "WithStatics" net10)
                    .Replace("private object _target;", "private static int s_extra; private object _target;")
                    .Replace (
                        "private object _invocationList;",
                        "private static object s_other; private object _invocationList;"
                    )

            let source =
                [
                    yield "using System;"
                    yield "namespace Fabricated"
                    yield "{"
                    yield! variants |> List.mapi (fun i variant -> pair $"%i{i}" variant)
                    yield withStatics
                    yield!
                        wrapperNearMisses
                        |> List.map (fun (name, wrapper) -> pairWith wrapper name net11)
                    yield "}"
                ]
                |> String.concat "\n"

            let image =
                Roslyn.compileAssembly "FabricatedDelegates" OutputKind.DynamicallyLinkedLibrary [] [ source ]

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory
            Assembly.read loggerFactory (Some "FabricatedDelegates") (new MemoryStream (image))

    let private typeNamed (assembly : DumpedAssembly) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        assembly.TypeDefs.Values
        |> Seq.filter (fun ty -> ty.Namespace = "Fabricated" && ty.Name = name)
        |> Seq.exactlyOne

    let private classifyPair (assembly : DumpedAssembly) (name : string) : Result<DelegateLayout, string> =
        DelegateLayout.classify
            assembly
            (typeNamed assembly $"Delegate%s{name}")
            (typeNamed assembly $"Multicast%s{name}")

    /// Which layout `result` recognised, by name.
    let private layoutKind (result : Result<DelegateLayout, string>) : string option =
        match result with
        | Ok (DelegateLayout.InvocationListAndCount _) -> Some "InvocationListAndCount"
        | Ok (DelegateLayout.HelperObjectAndExtraData _) -> Some "HelperObjectAndExtraData"
        | Error _ -> None

    let private isRecognised (result : Result<DelegateLayout, string>) : bool = (layoutKind result).IsSome

    let private refusalOf (result : Result<DelegateLayout, string>) : string =
        match result with
        | Ok layout -> failwith $"expected a refusal, got %A{layout}"
        | Error refusal -> refusal

    let private known : string =
        "(it knows System.Delegate { obj _target; obj _methodBase; intptr _methodPtr; intptr _methodPtrAux } with System.MulticastDelegate { obj _invocationList; intptr _invocationCount }, or System.Delegate { obj _helperObject; obj _target; intptr _methodPtr; intptr _methodPtrAux; intptr _extraData } with System.MulticastDelegate { } and System.Delegate.Wrapper { System.Delegate Value })"

    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    [<TestCaseSource(nameof runtimes)>]
    let ``each runtime's CoreLib has that runtime's layout`` (runtime : EmulatedRuntime) =
        let expected : (string * string) list =
            match runtime with
            | EmulatedRuntime.Net10 ->
                [
                    "Delegate", "_target"
                    "Delegate", "_methodPtr"
                    "Delegate", "_methodPtrAux"
                    "MulticastDelegate", "_invocationList"
                    "MulticastDelegate", "_invocationCount"
                ]
            | EmulatedRuntime.Net11 ->
                [
                    "Delegate", "_target"
                    "Delegate", "_methodPtr"
                    "Delegate", "_methodPtrAux"
                    "Delegate", "_helperObject"
                    "Delegate", "_extraData"
                    "Wrapper", "Value"
                ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let corelib =
            AssemblyApi.readFile loggerFactory (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let fields =
            match
                runtime,
                DelegateLayout.classify corelib baseClassTypes.DelegateType baseClassTypes.MulticastDelegateType
            with
            | EmulatedRuntime.Net10, Ok (DelegateLayout.InvocationListAndCount (binding, invocations)) ->
                [
                    binding.Target
                    binding.MethodPtr
                    binding.MethodPtrAux
                    invocations.InvocationList
                    invocations.InvocationCount
                ]
            | EmulatedRuntime.Net11, Ok (DelegateLayout.HelperObjectAndExtraData (binding, helper)) ->
                [
                    binding.Target
                    binding.MethodPtr
                    binding.MethodPtrAux
                    helper.HelperObject
                    helper.ExtraData
                    helper.WrapperValue
                ]
            | runtime, Ok other -> failwith $"%O{runtime}'s CoreLib has an unexpected delegate layout: %A{other}"
            | _, Error refusal -> failwith refusal

        fields
        |> List.map (fun field -> field.DeclaringType.Name, field.Name)
        |> shouldEqual expected

    [<Test>]
    let ``classify recognises exactly net10's and net11's field lists`` () =
        let assembly = fabricated.Force ()

        let recognised =
            variants
            |> List.mapi (fun i variant ->
                let result = classifyPair assembly $"%i{i}"

                let expected =
                    if variant = net10 then Some "InvocationListAndCount"
                    elif variant = net11 then Some "HelperObjectAndExtraData"
                    else None

                if layoutKind result <> expected then
                    failwith $"variant %i{i} %A{variant} classified as %A{result}; expected %A{expected}"

                expected
            )
            |> List.choose id
            |> List.length

        // Vacuity guard: both runtimes' lists are among the variants, and nothing else is accepted.
        recognised |> shouldEqual 2
        variants.Length |> shouldBeGreaterThan 100

    [<Test>]
    let ``static fields are not part of the layout`` () =
        classifyPair (fabricated.Force ()) "WithStatics"
        |> isRecognised
        |> shouldEqual true

    [<Test>]
    let ``net11's fields are refused without Delegate's own Wrapper of one Delegate Value`` () =
        let assembly = fabricated.Force ()

        let expectedWrapper : Map<string, string> =
            Map.ofList
                [
                    "NoWrapper", "Fabricated.DelegateNoWrapper declares no nested Wrapper"
                    "WrapperOfObject", "Fabricated.DelegateWrapperOfObject.Wrapper is { obj Value }"
                    "WrapperRenamed",
                    "Fabricated.DelegateWrapperRenamed.Wrapper is { Fabricated.DelegateWrapperRenamed Other }"
                    "WrapperWide",
                    "Fabricated.DelegateWrapperWide.Wrapper is { Fabricated.DelegateWrapperWide Value; int32 Extra }"
                    "WrapperOfOtherDelegate",
                    "Fabricated.DelegateWrapperOfOtherDelegate.Wrapper is { Fabricated.Delegate0 Value }"
                    "WrapperClass", "Fabricated.DelegateWrapperClass.Wrapper is not a value type"
                ]

        for name, _ in wrapperNearMisses do
            classifyPair assembly name
            |> refusalOf
            |> shouldEqual
                $"CoreLib declares Fabricated.Delegate%s{name} {{ obj _helperObject; obj _target; intptr _methodPtr; intptr _methodPtrAux; intptr _extraData }} with Fabricated.Multicast%s{name} {{ }}, but %s{expectedWrapper.[name]}, so it is not a delegate layout PawPrint knows how to write and read %s{known}"

    [<Test>]
    let ``near misses are refused, naming the fields found`` () =
        let assembly = fabricated.Force ()
        let delegateFields, multicastFields = net10

        let cases =
            [
                // `_invocationCount` retyped to a fixed-width integer.
                (delegateFields, replaceAt 1 ("long", "_invocationCount") multicastFields),
                "{ obj _target; obj _methodBase; intptr _methodPtr; intptr _methodPtrAux }",
                "{ obj _invocationList; int64 _invocationCount }"
                // `_methodPtr` and `_methodPtrAux` swapped.
                (replaceAt 2 delegateFields.[3] (replaceAt 3 delegateFields.[2] delegateFields), multicastFields),
                "{ obj _target; obj _methodBase; intptr _methodPtrAux; intptr _methodPtr }",
                "{ obj _invocationList; intptr _invocationCount }"
                // `_methodPtrAux` moved onto `MulticastDelegate`.
                (List.take 3 delegateFields, List.last delegateFields :: multicastFields),
                "{ obj _target; obj _methodBase; intptr _methodPtr }",
                "{ intptr _methodPtrAux; obj _invocationList; intptr _invocationCount }"
            ]

        for variant, delegateDescription, multicastDescription in cases do
            let index = List.findIndex ((=) variant) variants

            classifyPair assembly $"%i{index}"
            |> refusalOf
            |> shouldEqual
                $"CoreLib declares Fabricated.Delegate%i{index} %s{delegateDescription} with Fabricated.Multicast%i{index} %s{multicastDescription}, which is not a delegate layout PawPrint knows how to write and read %s{known}"

    /// A machine with a CoreLib, one of whose `System.Action`s has been allocated but not
    /// constructed. The delegate type is CoreLib's own, so the fixture binds no reference from
    /// another assembly to whichever CoreLib it has.
    type private DelegateFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            State : IlMachineState
            /// An `Action`, all of whose fields are zero.
            Delegate : ManagedHeapAddress
            /// `Action.Invoke`, as a method to bind the delegate to.
            Invoke : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        }

    let private makeDelegateFixtureOn (corelibPath : string) : DelegateFixture =
        let image =
            Roslyn.compileAssembly
                "DelegateLayoutTestGuest"
                OutputKind.DynamicallyLinkedLibrary
                []
                [ "public static class Entry { }" ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = AssemblyApi.readFile loggerFactory corelibPath
        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        use guestStream = new MemoryStream (image)
        let guest = AssemblyApi.read loggerFactory None guestStream

        let action =
            corelib.TypeDefs.Values
            |> Seq.find (fun ty -> ty.Namespace = "System" && ty.Name = "Action" && ty.Generics.IsEmpty)

        let state =
            (IlMachineState.initial loggerFactory ImmutableArray.Empty guest).WithLoadedAssembly corelib

        let concretise
            (state : IlMachineState)
            (assemblyName : string)
            (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : IlMachineState * ConcreteTypeHandle
            =
            LoadedTypeInfo.typeInfoToTypeDefn' baseClassTypes state.TypeSystem._LoadedAssemblies ty
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assemblyName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state =
            (state,
             [
                 baseClassTypes.Object
                 baseClassTypes.Int32
                 baseClassTypes.IntPtr
                 baseClassTypes.DelegateType
                 baseClassTypes.MulticastDelegateType
             ])
            ||> List.fold (fun state ty -> concretise state corelib.Name.FullName ty |> fst)

        let state, actionHandle = concretise state corelib.Name.FullName action

        let addr, state =
            IlMachineState.allocateUninitialisedInstance loggerFactory baseClassTypes actionHandle state

        let state, invoke =
            MulticastDelegateStub.invokeMethodOf loggerFactory baseClassTypes "test" actionHandle state

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            State = state
            Delegate = addr
            Invoke = invoke
        }

    let private makeDelegateFixture () : DelegateFixture =
        makeDelegateFixtureOn HostImage.coreLibPath

    /// The field of `layout` that holds a multicast delegate's count, and an open virtual
    /// delegate's `MethodDesc*`.
    let private countField (layout : DelegateLayout) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match layout with
        | DelegateLayout.InvocationListAndCount (_, invocations) -> invocations.InvocationCount
        | DelegateLayout.HelperObjectAndExtraData (_, helper) -> helper.ExtraData

    /// The field of `layout` that holds a multicast delegate's invocation list.
    let private listField (layout : DelegateLayout) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match layout with
        | DelegateLayout.InvocationListAndCount (_, invocations) -> invocations.InvocationList
        | DelegateLayout.HelperObjectAndExtraData (_, helper) -> helper.HelperObject

    let private setDelegateField
        (fixture : DelegateFixture)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap =
                ManagedHeap.setFieldById
                    fixture.Delegate
                    (DelegateLayout.fieldId state.TypeSystem.ConcreteTypes field)
                    value
                    state.ManagedHeap
        }

    let private nativeInt (source : NativeIntSource) : CliType =
        CliType.Numeric (CliNumericType.NativeInt source)

    /// An invocation list of `length` elements as `layout` holds one, each element naming
    /// `element`: an `object[]` of the delegates for net10's layout, a `Delegate.Wrapper[]` for
    /// net11's.
    let private invocationList
        (fixture : DelegateFixture)
        (element : ManagedHeapAddress option)
        (length : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match DelegateLayout.require fixture.BaseClassTypes with
        | DelegateLayout.InvocationListAndCount _ ->
            let objectType =
                AllConcreteTypes.getRequiredNonGenericHandle
                    state.TypeSystem.ConcreteTypes
                    fixture.BaseClassTypes.Object

            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectType)
                (fun () -> CliType.ObjectRef element)
                length
                state
        | DelegateLayout.HelperObjectAndExtraData (_, helper) ->
            let wrapperInfo =
                fixture.BaseClassTypes.Corelib.TypeDefs.Values
                |> Seq.filter (fun ty -> ty.Identity = helper.WrapperValue.DeclaringType.Identity)
                |> Seq.exactlyOne

            let state, wrapperType =
                LoadedTypeInfo.typeInfoToTypeDefn' fixture.BaseClassTypes state.TypeSystem._LoadedAssemblies wrapperInfo
                |> IlMachineState.concretizeType
                    fixture.LoggerFactory
                    fixture.BaseClassTypes
                    state
                    fixture.BaseClassTypes.Corelib.Name.FullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes wrapperType

            let wrapper =
                match zero with
                | CliType.ValueType vt ->
                    CliType.ValueType (CliValueType.WithFieldSet "Value" (CliType.ObjectRef element) vt)
                | other -> failwith $"expected Delegate.Wrapper's zero to be a value type, got %O{other}"

            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero wrapperType)
                (fun () -> wrapper)
                length
                state

    /// `baseClassTypes` with `Delegate` and `MulticastDelegate` exchanged: a CoreLib whose delegate
    /// types have some layout other than a known one.
    let private swapped (baseClassTypes : BaseClassTypes<DumpedAssembly>) : BaseClassTypes<DumpedAssembly> =
        { baseClassTypes with
            DelegateType = baseClassTypes.MulticastDelegateType
            MulticastDelegateType = baseClassTypes.DelegateType
        }

    let private swappedRefusal : string =
        "CoreLib declares System.MulticastDelegate { obj _invocationList; intptr _invocationCount } with System.Delegate { obj _target; obj _methodBase; intptr _methodPtr; intptr _methodPtrAux }, which is not a delegate layout PawPrint knows how to write and read"

    let private closedOverInvoke (fixture : DelegateFixture) : IlMachineState =
        DelegateRepresentation.write
            fixture.BaseClassTypes
            fixture.Delegate
            (DelegateBinding.Closed (None, FunctionPointerTarget.Managed fixture.Invoke))
            fixture.State

    [<TestCaseSource(nameof runtimes)>]
    let ``a delegate reads back through the layout as it was written`` (runtime : EmulatedRuntime) : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf runtime)

        // A count left over from before is overwritten: the binding decides every field it names.
        let fixture =
            { fixture with
                State =
                    setDelegateField
                        fixture
                        (countField (DelegateLayout.require fixture.BaseClassTypes))
                        (nativeInt (NativeIntSource.MethodHandlePtr 5L))
                        fixture.State
            }

        let state = closedOverInvoke fixture

        let invokeId, _ =
            MethodHandleRegistry.getOrAllocateConcreteId
                state.TypeSystem.ConcreteTypes
                fixture.Invoke
                state.MethodHandles

        // net11's binding records the target's `MethodDesc*`, as `Delegate_Construct` does.
        let expected =
            match DelegateLayout.require fixture.BaseClassTypes with
            | DelegateLayout.InvocationListAndCount _ -> DelegateInvocations.BindingOnly
            | DelegateLayout.HelperObjectAndExtraData _ -> DelegateInvocations.RecordedMethodDesc invokeId

        DelegateInvocations.read
            "test"
            fixture.BaseClassTypes
            state.TypeSystem.ConcreteTypes
            state.ManagedHeap
            fixture.Delegate
        |> shouldEqual expected

        match DelegateRepresentation.invocationOf fixture.BaseClassTypes "test" fixture.Delegate state with
        | DelegateInvocation.ThroughMethodPtr (None, FunctionPointerTarget.Managed method) ->
            method.Name |> shouldEqual "Invoke"
        | other -> failwith $"expected a closed invocation of Invoke, got %O{other}"

        let _, registryId =
            DelegateRepresentation.methodDescOf
                fixture.LoggerFactory
                fixture.BaseClassTypes
                "test"
                fixture.Delegate
                state

        let expected, _ =
            MethodHandleRegistry.getOrAllocateConcreteId
                state.TypeSystem.ConcreteTypes
                fixture.Invoke
                state.MethodHandles

        registryId |> shouldEqual expected

    let private stubThread : ThreadId = ThreadId.ThreadId 0

    /// A frame of `fixture.Delegate`'s type's multicast invoke stub, whose receiver is the
    /// delegate as `NewMulticastDelegate`'s `_target` makes it, installed as the active frame of
    /// `stubThread`.
    let private multicastStubFrame (fixture : DelegateFixture) (state : IlMachineState) : IlMachineState * MethodState =
        let delegateType =
            ManagedHeap.getObjectConcreteType fixture.Delegate state.ManagedHeap

        let state, stub =
            MulticastDelegateStub.synthesise fixture.LoggerFactory fixture.BaseClassTypes "test" delegateType state

        let frame =
            match
                MethodState.Empty
                    state.TypeSystem.ConcreteTypes
                    fixture.BaseClassTypes
                    state.TypeSystem._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    stub
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef (Some fixture.Delegate)))
                    None
            with
            | Ok frame -> frame
            | Error missing -> failwith $"unexpected missing assembly references: %O{missing}"

        { state with
            ThreadState = Map.empty |> Map.add stubThread (ThreadState.New frame)
        },
        frame

    [<Test>]
    let ``a CoreLib whose delegates have another layout is refused by every writer and reader`` () : unit =
        let fixture = makeDelegateFixture ()
        let state = closedOverInvoke fixture
        let other = swapped fixture.BaseClassTypes

        let written =
            Assert.Throws<System.Exception> (fun () ->
                DelegateRepresentation.write
                    other
                    fixture.Delegate
                    (DelegateBinding.Closed (None, FunctionPointerTarget.Managed fixture.Invoke))
                    fixture.State
                |> ignore<IlMachineState>
            )

        written.Message |> shouldContainText swappedRefusal

        let invoked =
            Assert.Throws<System.Exception> (fun () ->
                DelegateRepresentation.invocationOf other "test" fixture.Delegate state
                |> ignore<DelegateInvocation>
            )

        invoked.Message |> shouldContainText swappedRefusal

        let methodDesc =
            Assert.Throws<System.Exception> (fun () ->
                DelegateRepresentation.methodDescOf fixture.LoggerFactory other "test" fixture.Delegate state
                |> ignore<IlMachineState * int64>
            )

        methodDesc.Message |> shouldContainText swappedRefusal

        let state, frame = multicastStubFrame fixture state

        let stubRead =
            Assert.Throws<System.Exception> (fun () ->
                MulticastDelegateStub.execute fixture.LoggerFactory other stubThread frame state
                |> ignore<ExecutionResult>
            )

        stubRead.Message |> shouldContainText swappedRefusal

    [<TestCaseSource(nameof runtimes)>]
    let ``a delegate whose fields contradict its layout is refused`` (runtime : EmulatedRuntime) : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let state = closedOverInvoke fixture
        let count = countField (DelegateLayout.require fixture.BaseClassTypes)

        // The count is an `IntPtr`, so an object reference there is refused rather than read as
        // whatever it holds.
        let state =
            setDelegateField fixture count (CliType.ObjectRef (Some fixture.Delegate)) state

        let e =
            Assert.Throws<System.Exception> (fun () ->
                DelegateRepresentation.methodDescOf
                    fixture.LoggerFactory
                    fixture.BaseClassTypes
                    "test"
                    fixture.Delegate
                    state
                |> ignore<IlMachineState * int64>
            )

        e.Message
        |> shouldContainText $"test: expected %s{count.Name} to be a native int, got"

    [<TestCaseSource(nameof runtimes)>]
    let ``the multicast invoke stub reads its elements through the layout`` (runtime : EmulatedRuntime) : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let layout = DelegateLayout.require fixture.BaseClassTypes

        // Two elements, neither of them naming a delegate: the stub must reach element 0 through
        // the layout's count and list, and refuse it as the layout's list cannot hold it.
        let list, state = invocationList fixture None 2 fixture.State

        let state =
            state
            |> setDelegateField fixture (listField layout) (CliType.ObjectRef (Some list))
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.Verbatim 2L))

        let state, frame = multicastStubFrame fixture state

        let e =
            Assert.Throws<System.Exception> (fun () ->
                MulticastDelegateStub.execute fixture.LoggerFactory fixture.BaseClassTypes stubThread frame state
                |> ignore<ExecutionResult>
            )

        e.Message
        |> shouldEqual
            "multicast delegate invoke stub: expected element 0 of the invocation list to name a delegate, got ObjectRef None"

    [<TestCaseSource(nameof runtimes)>]
    let ``a multicast delegate's list and count are read through the layout`` (runtime : EmulatedRuntime) : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let layout = DelegateLayout.require fixture.BaseClassTypes
        let list, state = invocationList fixture (Some fixture.Delegate) 3 fixture.State

        let state =
            state
            |> setDelegateField fixture (listField layout) (CliType.ObjectRef (Some list))
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.Verbatim 2L))

        match
            DelegateInvocations.read
                "test"
                fixture.BaseClassTypes
                state.TypeSystem.ConcreteTypes
                state.ManagedHeap
                fixture.Delegate
        with
        | DelegateInvocations.Multicast (readList, count, elements) ->
            readList |> shouldEqual list
            count |> shouldEqual 2L

            for index in 0..2 do
                DelegateInvocations.element
                    "test"
                    state.TypeSystem.ConcreteTypes
                    elements
                    readList
                    index
                    state.ManagedHeap
                |> shouldEqual fixture.Delegate
        | other -> failwith $"expected a multicast delegate, got %O{other}"

    [<TestCaseSource(nameof runtimes)>]
    let ``a MethodDesc in the count is read through the layout`` (runtime : EmulatedRuntime) : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let layout = DelegateLayout.require fixture.BaseClassTypes

        let state =
            closedOverInvoke fixture
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.MethodHandlePtr 5L))

        // On net10 only an open virtual delegate has one; on net11 every single-cast delegate may.
        let expected =
            match layout with
            | DelegateLayout.InvocationListAndCount _ -> DelegateInvocations.OpenVirtual 5L
            | DelegateLayout.HelperObjectAndExtraData _ -> DelegateInvocations.RecordedMethodDesc 5L

        DelegateInvocations.read
            "test"
            fixture.BaseClassTypes
            state.TypeSystem.ConcreteTypes
            state.ManagedHeap
            fixture.Delegate
        |> shouldEqual expected

    [<TestCaseSource(nameof runtimes)>]
    let ``an open virtual delegate reads back through the layout as it was written``
        (runtime : EmulatedRuntime)
        : unit
        =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let state =
            DelegateRepresentation.write
                fixture.BaseClassTypes
                fixture.Delegate
                (DelegateBinding.Open (
                    FunctionPointerTarget.VirtualCallStub (VirtualDispatchToken.ClassSlot 0, fixture.Invoke)
                ))
                fixture.State

        let invokeId, _ =
            MethodHandleRegistry.getOrAllocateConcreteId
                state.TypeSystem.ConcreteTypes
                fixture.Invoke
                state.MethodHandles

        let expected =
            match DelegateLayout.require fixture.BaseClassTypes with
            | DelegateLayout.InvocationListAndCount _ -> DelegateInvocations.OpenVirtual invokeId
            | DelegateLayout.HelperObjectAndExtraData _ -> DelegateInvocations.RecordedMethodDesc invokeId

        DelegateInvocations.read
            "test"
            fixture.BaseClassTypes
            state.TypeSystem.ConcreteTypes
            state.ManagedHeap
            fixture.Delegate
        |> shouldEqual expected

        DelegateRepresentation.methodDescOf fixture.LoggerFactory fixture.BaseClassTypes "test" fixture.Delegate state
        |> snd
        |> shouldEqual invokeId

    [<Test>]
    let ``net11's recorded MethodDesc is the delegate's method, whatever the binding fields say`` () : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf EmulatedRuntime.Net11)

        let layout = DelegateLayout.require fixture.BaseClassTypes

        // A closed delegate, whose `_methodPtrAux` is zero, with a cached `MethodDesc*`.
        let state =
            closedOverInvoke fixture
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.MethodHandlePtr 5L))

        DelegateRepresentation.methodDescOf fixture.LoggerFactory fixture.BaseClassTypes "test" fixture.Delegate state
        |> snd
        |> shouldEqual 5L

    [<Test>]
    let ``net11's unmanaged function pointer delegate is one PawPrint does not build`` () : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf EmulatedRuntime.Net11)

        let layout = DelegateLayout.require fixture.BaseClassTypes

        let state =
            closedOverInvoke fixture
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.Verbatim -1L))

        match
            DelegateInvocations.read
                "test"
                fixture.BaseClassTypes
                state.TypeSystem.ConcreteTypes
                state.ManagedHeap
                fixture.Delegate
        with
        | DelegateInvocations.Unbuilt _ -> ()
        | other -> failwith $"expected an unmanaged function pointer delegate to be unbuilt, got %O{other}"

    [<Test>]
    let ``net11's _helperObject is an invocation list only when it is a Delegate.Wrapper[]`` () : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf EmulatedRuntime.Net11)

        let layout = DelegateLayout.require fixture.BaseClassTypes

        let read (state : IlMachineState) : DelegateInvocations =
            DelegateInvocations.read
                "test"
                fixture.BaseClassTypes
                state.TypeSystem.ConcreteTypes
                state.ManagedHeap
                fixture.Delegate

        // `_helperObject` also caches `Delegate.Method`, and an open virtual delegate's
        // `_extraData` names its target regardless.
        let state =
            closedOverInvoke fixture
            |> setDelegateField fixture (listField layout) (CliType.ObjectRef (Some fixture.Delegate))
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.MethodHandlePtr 5L))

        read state |> shouldEqual (DelegateInvocations.RecordedMethodDesc 5L)

        // An array that is not a `Wrapper[]` is not an invocation list, whatever the count says,
        // even once `Wrapper` itself is a type the machine has made arrays of.
        let _, state = invocationList fixture None 1 state

        let objectType =
            AllConcreteTypes.getRequiredNonGenericHandle state.TypeSystem.ConcreteTypes fixture.BaseClassTypes.Object

        let objects, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectType)
                (fun () -> CliType.ObjectRef (Some fixture.Delegate))
                2
                state

        let state =
            state
            |> setDelegateField fixture (listField layout) (CliType.ObjectRef (Some objects))
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.Verbatim 2L))

        match read state with
        | DelegateInvocations.Unbuilt _ -> ()
        | other -> failwith $"expected an object[] in _helperObject not to be read as an invocation list, got %O{other}"

    [<Test>]
    let ``net10's _invocationList beside a MethodDesc is not a delegate PawPrint builds`` () : unit =
        let fixture =
            makeDelegateFixtureOn (FrameworkUnderTest.requireCoreLibPathOf EmulatedRuntime.Net10)

        let layout = DelegateLayout.require fixture.BaseClassTypes

        let state =
            closedOverInvoke fixture
            |> setDelegateField fixture (listField layout) (CliType.ObjectRef (Some fixture.Delegate))
            |> setDelegateField fixture (countField layout) (nativeInt (NativeIntSource.MethodHandlePtr 5L))

        match
            DelegateInvocations.read
                "test"
                fixture.BaseClassTypes
                state.TypeSystem.ConcreteTypes
                state.ManagedHeap
                fixture.Delegate
        with
        | DelegateInvocations.Unbuilt _ -> ()
        | other -> failwith $"expected a wrapper delegate to be one PawPrint does not build, got %O{other}"
