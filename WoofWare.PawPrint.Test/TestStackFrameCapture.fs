namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// <c>StackFrameCapture.ofThread</c>: the walk that turns a thread's live frames into the frames a
/// captured stack trace reports.
/// </summary>
/// <remarks>
/// The offsets asserted here are literals this fixture writes into the frames it builds, not values
/// read back out of the same structures, so the expectation does not share a mistake with the
/// implementation. The call-site offset and the caller's own program counter are deliberately set
/// to *different* numbers in every chain below: they coincide in real guests compiled unoptimized
/// (Roslyn emits a `nop` after each call statement, so both land in one sequence point), and a test
/// where they coincided could not tell the correct rule from the resume-point mutant.
/// </remarks>
[<TestFixture>]
module TestStackFrameCapture =

    // The factory is intentionally not disposed: the DumpedAssembly keeps the logger.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    /// A frame whose method is an ordinary metadata-backed one, so it is reported rather than
    /// suppressed. Which method it is does not matter to the walk; only the offsets and the return
    /// chain do.
    let private aMethod () : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = concreteTypes
            }

        let ctor =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = ".ctor" && (MethodInfo.arity method = 0))

        let _, signature =
            IlMachineState.concretizeMethodSignature
                loggerFactory
                bct
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                ctor.Signature

        let ops : (IlOp * int) list =
            [ IlOp.Nullary NullaryIlOp.Nop, 0 ; IlOp.Nullary NullaryIlOp.Ret, 1 ]

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            {
                Instructions = ops
                Locations = ops |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
                LocalsInit = false
                LocalVars = None
                ExceptionRegions = ImmutableArray.Empty
            }

        ctor
        |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::.ctor is not type-generic")
        |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::.ctor is not method-generic")
        |> MethodInfo.setMethodVars (MethodBody.Il instructions) signature

    let private emptyFrame () : MethodState =
        match
            MethodState.Empty
                concreteTypes
                bct
                loaded
                corelib
                (aMethod ())
                ImmutableArray.Empty
                (ImmutableArray.Create (CliType.ObjectRef None))
                None
        with
        | Ok methodState -> methodState
        | Error missing -> failwith $"Unexpected missing assembly references building a capture test frame: %O{missing}"

    /// A thread whose live stack is, innermost last, the frames described by `chain`: each entry is
    /// the program counter that frame is sitting at, paired with the call-site offset recorded by
    /// the frame it called. The outermost frame has no callee, so its call-site entry is unused.
    ///
    /// Returns the thread with the innermost frame active.
    let private threadWithChain (chain : (int * int) list) : ThreadState =
        match chain with
        | [] -> failwith "a thread must have at least one frame"
        | (outermostPc, _) :: rest ->

        let outermost =
            { emptyFrame () with
                ReturnState = None
            }
            |> MethodState.setProgramCounter outermostPc

        let thread = ThreadState.New outermost

        // Walk outermost-to-innermost, pushing each frame as the callee of the one before it and
        // recording on the callee the call site its caller is parked on.
        (((thread, thread.ActiveMethodState), chain |> List.map snd), rest)
        ||> List.fold (fun ((thread, callerFrameId), callSites) (pc, _) ->
            let callSite =
                match callSites with
                | site :: _ -> site
                | [] -> failwith "chain and call-site list disagree in length"

            let returnState : MethodReturnState =
                {
                    JumpTo = callerFrameId
                    WasInitialisingType = None
                    Constructing = ConstructionState.NotConstructing
                    CallSiteIlOpIndex = callSite
                    ReturnValueDisposition = ReturnValueDisposition.PushToCaller
                    WrapExceptionInTargetInvocation = false
                }

            let callee =
                { emptyFrame () with
                    ReturnState = Some returnState
                }
                |> MethodState.setProgramCounter pc

            let calleeFrameId, thread = ThreadState.appendFrame callee thread
            let thread = ThreadState.setActiveFrame calleeFrameId thread

            (thread, calleeFrameId), (callSites |> List.tail)
        )
        |> fst
        |> fst

    [<Test>]
    let ``a single-frame thread reports that frame at its program counter`` () =
        let thread = threadWithChain [ 17, 99 ]

        let frames = StackFrameCapture.ofThread thread

        frames |> List.length |> shouldEqual 1
        frames.[0].IlOffset |> shouldEqual 17

    [<Test>]
    let ``frames are reported innermost first`` () =
        // Outermost sitting at 40 having called from 30; middle at 20 having called from 10;
        // innermost at 5.
        let thread = threadWithChain [ 40, 30 ; 20, 10 ; 5, 0 ]

        let frames = StackFrameCapture.ofThread thread

        frames |> List.length |> shouldEqual 3

        // The innermost frame is the active one, at its own PC.
        frames.[0].IlOffset |> shouldEqual 5

    [<Test>]
    let ``an enclosing frame is reported at its call site, not at the offset it will resume at`` () =
        // The middle frame's program counter is 20, but the callee it is waiting on recorded its
        // call site as 10. A capture must say 10. The two differ by more than the width of any
        // instruction so that reporting either one is unambiguous.
        let thread = threadWithChain [ 40, 30 ; 20, 10 ; 5, 0 ]

        let frames = StackFrameCapture.ofThread thread

        frames.[1].IlOffset |> shouldEqual 10
        frames.[2].IlOffset |> shouldEqual 30

        // Stated as an inequality too: these are the values the resume-point mutant would produce,
        // so a chain where they happened to coincide would make this fixture vacuous.
        frames.[1].IlOffset |> shouldNotEqual 20
        frames.[2].IlOffset |> shouldNotEqual 40

    [<Test>]
    let ``no frame of a live capture is marked as ending an earlier trace`` () =
        let thread = threadWithChain [ 40, 30 ; 20, 10 ; 5, 0 ]

        StackFrameCapture.ofThread thread
        |> List.forall (fun frame -> not frame.IsLastFrameFromForeignExceptionStackTrace)
        |> shouldEqual true

    [<Test>]
    let ``every reported frame carries the method its own frame was executing`` () =
        let thread = threadWithChain [ 40, 30 ; 20, 10 ; 5, 0 ]

        let frames = StackFrameCapture.ofThread thread

        // Every frame in these synthetic chains runs the same method, so this checks only that the
        // walk reads the method off the frame rather than substituting the active one's; the
        // guest-level `StackTraceCurrentThreadFrames.cs` is what distinguishes the methods by name.
        frames
        |> List.forall (fun frame -> frame.Method.Name = ".ctor")
        |> shouldEqual true

    [<Test>]
    let ``a capture refuses while a first-pass search is suspended in a filter`` () =
        // `ExceptionDispatching.firstPass` moves the active frame outward to the frame hosting the
        // filter and leaves every frame inner to it live — it is a search, not an unwind. Those
        // frames are not on the host's return chain, so a walk from the host would report a trace
        // with the throw missing from it. Real .NET includes them (measured), so answering
        // partially would be wrong rather than merely incomplete.
        let thread = threadWithChain [ 40, 30 ; 20, 10 ; 5, 0 ]

        let filterRegion : ExceptionFilterRegion =
            {
                FilterOffset = 3
                HandlerOffset =
                    {
                        TryOffset = 0
                        TryLength = 4
                        HandlerOffset = 10
                        HandlerLength = 1
                    }
            }

        let continuationFrame : ExceptionContinuationFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Scope = ExceptionContinuationScope.FilterHandler filterRegion
                Continuation =
                    ExceptionContinuation.ResumeAfterFilter
                        {
                            CurrentFilter = filterRegion
                            Search =
                                {
                                    Exception =
                                        {
                                            ExceptionObject = ManagedHeapAddress 1
                                            StackTrace = []
                                        }
                                    ExceptionType = ConcreteTypeHandle.Concrete 0
                                    StartFrame = thread.ActiveMethodState
                                    StartPC = 0
                                    Frame = thread.ActiveMethodState
                                    SearchPC = 0
                                    SkippedFilters = []
                                }
                        }
            }

        let withFilter =
            ThreadState.mapFrame
                thread.ActiveMethodState
                (fun frame ->
                    { frame with
                        ExceptionContinuations = [ continuationFrame ]
                    }
                )
                thread

        let exn =
            Assert.Throws<System.Exception> (fun () -> StackFrameCapture.ofThread withFilter |> ignore)

        exn.Message |> shouldContainText "suspended in a filter"

        // Without the continuation the same chain walks fine, so the refusal is caused by the
        // filter state rather than by anything else about the fixture.
        StackFrameCapture.ofThread thread |> List.length |> shouldEqual 3
