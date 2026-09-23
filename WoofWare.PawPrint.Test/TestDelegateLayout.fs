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
    /// field moved across the `Delegate`/`MulticastDelegate` boundary, and net11's.
    let private variants : Variant list =
        let delegateFields, multicastFields = net10

        [
            yield net10
            for edited in oneEditFrom delegateFields do
                yield edited, multicastFields
            for edited in oneEditFrom multicastFields do
                yield delegateFields, edited
            yield List.take 3 delegateFields, List.last delegateFields :: multicastFields
            yield delegateFields @ [ List.head multicastFields ], List.tail multicastFields
            yield net11
        ]
        |> List.distinct

    let private classBody (fields : (string * string) list) : string =
        fields
        |> List.map (fun (ty, field) -> $"        private %s{ty} %s{field};")
        |> String.concat "\n"

    /// A `Delegate{i}` and a `Multicast{i} : Delegate{i}` per variant, and a pair carrying net10's
    /// fields together with statics.
    let private fabricated : Lazy<DumpedAssembly> =
        lazy
            let pair (name : string) ((delegateFields, multicastFields) : Variant) : string =
                $"    internal abstract class Delegate%s{name}\n    {{\n%s{classBody delegateFields}\n    }}\n"
                + $"    internal abstract class Multicast%s{name} : Delegate%s{name}\n    {{\n%s{classBody multicastFields}\n    }}"

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

    let private isRecognised (result : Result<DelegateLayout, string>) : bool =
        match result with
        | Ok (DelegateLayout.InvocationListAndCount _) -> true
        | Error _ -> false

    let private refusalOf (result : Result<DelegateLayout, string>) : string =
        match result with
        | Ok layout -> failwith $"expected a refusal, got %A{layout}"
        | Error refusal -> refusal

    let private known : string =
        "(it knows System.Delegate { obj _target; obj _methodBase; intptr _methodPtr; intptr _methodPtrAux } with System.MulticastDelegate { obj _invocationList; intptr _invocationCount })"

    let private readHostCorelib (loggerFactory : ILoggerFactory) : DumpedAssembly =
        AssemblyApi.readFile loggerFactory typeof<obj>.Assembly.Location

    [<Test>]
    let ``the host's CoreLib has the invocation-list-and-count layout`` () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let corelib = readHostCorelib loggerFactory
        let baseClassTypes = Corelib.getBaseTypes corelib

        match DelegateLayout.classify corelib baseClassTypes.DelegateType baseClassTypes.MulticastDelegateType with
        | Ok (DelegateLayout.InvocationListAndCount (binding, invocations)) ->
            [
                binding.Target
                binding.MethodPtr
                binding.MethodPtrAux
                invocations.InvocationList
                invocations.InvocationCount
            ]
            |> List.map (fun field -> field.DeclaringType.Name, field.Name)
            |> shouldEqual
                [
                    "Delegate", "_target"
                    "Delegate", "_methodPtr"
                    "Delegate", "_methodPtrAux"
                    "MulticastDelegate", "_invocationList"
                    "MulticastDelegate", "_invocationCount"
                ]
        | Error refusal -> failwith refusal

    [<Test>]
    let ``classify recognises exactly net10's field lists`` () =
        let assembly = fabricated.Force ()

        let recognised =
            variants
            |> List.mapi (fun i variant ->
                let result = classifyPair assembly $"%i{i}"
                let expected = (variant = net10)

                if isRecognised result <> expected then
                    failwith $"variant %i{i} %A{variant} classified as %A{result}; expected recognised=%b{expected}"

                expected
            )
            |> List.filter id
            |> List.length

        // Vacuity guard: net10's own lists are among the variants, and nothing else is accepted.
        recognised |> shouldEqual 1
        variants.Length |> shouldBeGreaterThan 50

    [<Test>]
    let ``static fields are not part of the layout`` () =
        classifyPair (fabricated.Force ()) "WithStatics"
        |> isRecognised
        |> shouldEqual true

    [<Test>]
    let ``net11's layout is refused, naming the fields found`` () =
        let index = List.findIndex ((=) net11) variants

        classifyPair (fabricated.Force ()) $"%i{index}"
        |> refusalOf
        |> shouldEqual
            $"CoreLib declares Fabricated.Delegate%i{index} {{ obj _helperObject; obj _target; intptr _methodPtr; intptr _methodPtrAux; intptr _extraData }} with Fabricated.Multicast%i{index} {{ }}, which is not a delegate layout PawPrint knows how to write and read %s{known}"

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

    /// A machine with the host's CoreLib and a guest declaring `IntFunc`, one of whose instances
    /// has been allocated but not constructed.
    type private DelegateFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Guest : DumpedAssembly
            State : IlMachineState
            /// An `IntFunc`, all of whose fields are zero.
            Delegate : ManagedHeapAddress
            /// `IntFunc.Invoke`, as a method to bind the delegate to.
            Invoke : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        }

    let private makeDelegateFixture () : DelegateFixture =
        let image =
            Roslyn.compileAssembly
                "DelegateLayoutTestGuest"
                OutputKind.DynamicallyLinkedLibrary
                []
                [ "public delegate int IntFunc(int x);" ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = readHostCorelib loggerFactory
        let baseClassTypes = Corelib.getBaseTypes corelib
        use guestStream = new MemoryStream (image)
        let guest = AssemblyApi.read loggerFactory None guestStream

        let intFunc = guest.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = "IntFunc")

        let state =
            (IlMachineState.initial loggerFactory ImmutableArray.Empty guest).WithLoadedAssembly corelib

        let concretise
            (state : IlMachineState)
            (assemblyName : string)
            (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : IlMachineState * ConcreteTypeHandle
            =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies ty
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

        let state, intFuncHandle = concretise state guest.Name.FullName intFunc

        let addr, state =
            IlMachineState.allocateUninitialisedInstance loggerFactory baseClassTypes intFuncHandle state

        let state, invoke =
            MulticastDelegateStub.invokeMethodOf loggerFactory baseClassTypes "test" intFuncHandle state

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Guest = guest
            State = state
            Delegate = addr
            Invoke = invoke
        }

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

    [<Test>]
    let ``a delegate reads back through the layout as it was written`` () : unit =
        let fixture = makeDelegateFixture ()
        let state = closedOverInvoke fixture

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
            MethodHandleRegistry.getOrAllocateConcreteId state.ConcreteTypes fixture.Invoke state.MethodHandles

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
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.Guest
                    stub
                    ImmutableArray.Empty
                    (ImmutableArray.Create (
                        CliType.ObjectRef (Some fixture.Delegate),
                        CliType.Numeric (CliNumericType.Int32 0)
                    ))
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

    [<Test>]
    let ``a delegate whose fields contradict its layout is refused`` () : unit =
        let fixture = makeDelegateFixture ()
        let state = closedOverInvoke fixture

        let invocations =
            match DelegateLayout.require fixture.BaseClassTypes with
            | DelegateLayout.InvocationListAndCount (_, invocations) -> invocations

        // `_invocationCount` is an `IntPtr`, so an object reference there is refused rather than
        // read as whatever it holds.
        let heap =
            state.ManagedHeap
            |> ManagedHeap.setFieldById
                fixture.Delegate
                (DelegateLayout.fieldId state.ConcreteTypes invocations.InvocationCount)
                (CliType.ObjectRef (Some fixture.Delegate))

        let state =
            { state with
                ManagedHeap = heap
            }

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
        |> shouldContainText "test: expected _invocationCount to be a native int, got"

    [<Test>]
    let ``the multicast invoke stub reads its elements through the layout`` () : unit =
        let fixture = makeDelegateFixture ()

        let invocations =
            match DelegateLayout.require fixture.BaseClassTypes with
            | DelegateLayout.InvocationListAndCount (_, invocations) -> invocations

        let objectType =
            AllConcreteTypes.getRequiredNonGenericHandle fixture.State.ConcreteTypes fixture.BaseClassTypes.Object

        // Two elements, neither of them a delegate: the stub must reach element 0 through the
        // layout's count and list, and refuse it as the layout's list cannot hold it.
        let list, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectType)
                (fun () -> CliType.ObjectRef None)
                2
                fixture.State

        let set (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) (value : CliType) (heap : ManagedHeap) =
            ManagedHeap.setFieldById fixture.Delegate (DelegateLayout.fieldId state.ConcreteTypes field) value heap

        let state =
            { state with
                ManagedHeap =
                    state.ManagedHeap
                    |> set invocations.InvocationList (CliType.ObjectRef (Some list))
                    |> set
                        invocations.InvocationCount
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 2L)))
            }

        let state, frame = multicastStubFrame fixture state

        let e =
            Assert.Throws<System.Exception> (fun () ->
                MulticastDelegateStub.execute fixture.LoggerFactory fixture.BaseClassTypes stubThread frame state
                |> ignore<ExecutionResult>
            )

        e.Message
        |> shouldEqual
            "multicast delegate invoke stub: expected element 0 of _invocationList to reference a delegate, got ObjectRef None"
