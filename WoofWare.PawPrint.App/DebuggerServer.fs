namespace WoofWare.PawPrint

open System
open System.Buffers
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Net
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Hosting.Server
open Microsoft.AspNetCore.Hosting.Server.Features
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers
open WoofWare.PosixKernel

[<RequireQualifiedAccess>]
module DebuggerServer =
    let private threadIdValue (threadId : ThreadId) : int =
        match threadId with
        | ThreadId.ThreadId i -> i

    let private frameIdValue (frameId : FrameId) : int =
        match frameId with
        | FrameId.FrameId i -> i

    let private heapAddressValue (address : ManagedHeapAddress) : int =
        match address with
        | ManagedHeapAddress.ManagedHeapAddress i -> i

    let private pathSegments (path : string) : string list =
        path.TrimEnd('/').Split ('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map WebUtility.UrlDecode
        |> Array.toList

    let private currentInstruction (frame : MethodState) : string option =
        match MethodInfo.tryIlBody frame.ExecutingMethod with
        | None -> None
        | Some instructions -> instructions.Locations |> Map.tryFind frame.IlOpIndex |> Option.map string

    let private writeOptionalString (writer : Utf8JsonWriter) (name : string) (value : string option) : unit =
        writer.WritePropertyName name

        match value with
        | Some value -> writer.WriteStringValue value
        | None -> writer.WriteNullValue ()

    let private writeOptionalInt (writer : Utf8JsonWriter) (name : string) (value : int option) : unit =
        writer.WritePropertyName name

        match value with
        | Some value -> writer.WriteNumberValue value
        | None -> writer.WriteNullValue ()

    let private writeOptionalHeapAddress
        (writer : Utf8JsonWriter)
        (name : string)
        (value : ManagedHeapAddress option)
        : unit
        =
        writer.WritePropertyName name

        match value with
        | Some value -> writer.WriteNumberValue (heapAddressValue value)
        | None -> writer.WriteNullValue ()

    /// `task` is the emulated kernel's record for this thread, which is where a syscall park
    /// keeps both which syscall it is in and what it is waiting for: `BlockedInSyscall`
    /// carries neither, so a renderer handed only the status could say that a thread is
    /// parked and nothing else. `None` — a thread with no task at all — is an interpreter
    /// bug that `EmulatedKernel.checkTaskInvariants` names; this reports it rather than
    /// raising, because a debugger is most wanted when the machine is already wrong.
    let private writeThreadStatus
        (writer : Utf8JsonWriter)
        (task : UnixTaskState option)
        (status : ThreadStatus)
        : unit
        =
        match status with
        | ThreadStatus.Runnable -> writer.WriteStringValue "runnable"
        | ThreadStatus.NotStarted -> writer.WriteStringValue "notStarted"
        | ThreadStatus.BlockedOnJoin (target, deadlineTicks) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnJoin")
            writer.WriteNumber ("targetThread", threadIdValue target)

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnClassInit blocker ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnClassInit")
            writer.WriteNumber ("blockerThread", threadIdValue blocker)
            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnMonitorAcquire (LowLevelMonitorId monitor) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnMonitorAcquire")
            writer.WriteNumber ("monitor", monitor)
            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnMonitorWait (LowLevelMonitorId monitor, deadlineTicks) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnMonitorWait")
            writer.WriteNumber ("monitor", monitor)

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnSyncBlockAcquire (lockObject, deadlineTicks) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnSyncBlockAcquire")
            writer.WriteNumber ("lockObject", heapAddressValue lockObject)

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnSyncBlockWait (lockObject, deadlineTicks) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnSyncBlockWait")
            writer.WriteNumber ("lockObject", heapAddressValue lockObject)

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnWaitHandle (WaitHandleId handle, deadlineTicks, _) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnWaitHandle")
            writer.WriteNumber ("handle", handle)

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnWaitHandles (handles, waitAll, deadlineTicks) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnWaitHandles")
            writer.WriteBoolean ("waitAll", waitAll)
            writer.WriteStartArray "handles"

            for WaitHandleId handle in handles do
                writer.WriteNumberValue handle

            writer.WriteEndArray ()

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedOnSleep deadlineTicks ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "blockedOnSleep")

            match deadlineTicks with
            | None -> ()
            | Some ticks -> writer.WriteNumber ("deadlineTicks", ticks)

            writer.WriteEndObject ()
        | ThreadStatus.BlockedInSyscall ->
            writer.WriteStartObject ()

            // `kind` names the syscall, which the status no longer does: the
            // record is where that lives, and reporting "blockedInSyscall" for
            // every park would tell a debugger client strictly less than it used
            // to be told. The wire shapes are therefore unchanged for a
            // well-formed park, and the kind-less object below is reached only in
            // a state `EmulatedKernel.checkTaskInvariants` calls a defect.
            //
            // Both objects are written as the open file *description*, which is
            // what a park holds: an epoll instance is a description, so a `dup`'d
            // port waits on the same one, and the descriptor number the guest
            // called through may since have been closed or reused.
            match task |> Option.bind (fun task -> task.Parked) with
            | Some (ParkedSyscall.SocketWait wait) ->
                writer.WriteString ("kind", "blockedOnSocketEvents")
                let (OpenFileDescriptionId port) = wait.Port
                writer.WriteNumber ("port", port)
            | Some (ParkedSyscall.Flock parked) ->
                writer.WriteString ("kind", "blockedOnFlock")
                let (OpenFileDescriptionId description) = parked.Requester
                writer.WriteNumber ("description", description)

                writer.WriteString (
                    "mode",
                    match parked.Mode with
                    | FlockMode.Shared -> "shared"
                    | FlockMode.Exclusive -> "exclusive"
                )
            | None -> writer.WriteString ("kind", "blockedInSyscall")

            writer.WriteEndObject ()
        | ThreadStatus.Terminated -> writer.WriteStringValue "terminated"
        | ThreadStatus.Parked -> writer.WriteStringValue "parked"
        | ThreadStatus.WaitingForForegroundThreads -> writer.WriteStringValue "waitingForForegroundThreads"

    /// `location` as the span resolved at `sourceIlOffset`.
    let private writeSourceLocationValue
        (writer : Utf8JsonWriter)
        (sourceIlOffset : int)
        (location : SourceLocation)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteNumber ("ilOffset", sourceIlOffset)
        // Exactly as the PDB records it — an absolute path on whichever machine built the
        // assembly. Deliberately not resolved against this filesystem; see `SourceLocation`.
        writer.WriteString ("documentPath", location.DocumentPath)
        writer.WriteNumber ("startLine", location.StartLine)
        writer.WriteNumber ("startColumn", location.StartColumn)
        writer.WriteNumber ("endLine", location.EndLine)
        writer.WriteNumber ("endColumn", location.EndColumn)
        writer.WriteEndObject ()

    /// The source span the compiler attributed to `sourceIlOffset` in `frame`, or `null`.
    ///
    /// `null` is ordinary: the shared framework ships without PDBs, a synthesised stub has no
    /// metadata row to key one by, and a compiler may mark a range as having no source at all.
    ///
    /// The span carries the offset it was resolved at, which for anything but the active frame
    /// is *not* the frame's `ilOffset`: see `GuestLocation.attributionOffsets`.
    let private writeSourceLocation
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (sourceIlOffset : int)
        (frame : MethodState)
        : unit
        =
        writer.WritePropertyName "sourceLocation"

        match GuestLocation.trySourceOf state frame.ExecutingMethod sourceIlOffset with
        | None -> writer.WriteNullValue ()
        | Some location -> writeSourceLocationValue writer sourceIlOffset location

    let private writeFrameProperties
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (sourceIlOffset : int)
        (includeActive : bool)
        (activeFrame : FrameId)
        (frameId : FrameId)
        (frame : MethodState)
        : unit
        =
        writer.WriteNumber ("id", frameIdValue frameId)

        if includeActive then
            writer.WriteBoolean ("active", (frameId = activeFrame))

        writer.WriteString ("method", string frame.ExecutingMethod)
        writer.WriteNumber ("ilOffset", frame.IlOpIndex)
        writeOptionalString writer "instruction" (currentInstruction frame)
        writeSourceLocation writer state sourceIlOffset frame
        writer.WriteNumber ("evalStackDepth", frame.EvaluationStack.Values.Length)
        writer.WriteNumber ("argumentCount", frame.Arguments.Length)
        writer.WriteNumber ("localCount", frame.LocalVariables.Length)

    let private writeFrameSummary
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (sourceIlOffset : int)
        (activeFrame : FrameId)
        (frameId : FrameId)
        (frame : MethodState)
        : unit
        =
        writer.WriteStartObject ()
        writeFrameProperties writer state sourceIlOffset false activeFrame frameId frame
        writer.WriteEndObject ()

    let private writeThreadSummary
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (threadId : ThreadId)
        (threadState : ThreadState)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteNumber ("id", threadIdValue threadId)
        writer.WritePropertyName "status"
        writeThreadStatus writer (state.Kernel.Tasks |> Map.tryFind threadId) threadState.Status

        if ThreadStatus.hasNoActiveFrame threadState.Status then
            // A frameless thread (pre-`Start`, or a kernel-owned Parked
            // dispatcher) has no live frame; the ActiveMethodState/MethodState
            // accessors would crash on the sentinel `FrameId -1`. Surface the
            // absence explicitly rather than skipping the keys, so consumers
            // see a consistent shape.
            writer.WriteNull "activeAssembly"
            writer.WriteNull "activeFrame"
            writer.WriteNull "activeFrameSummary"
        else
            writer.WriteString ("activeAssembly", threadState.ActiveAssemblyFullName)
            writer.WriteNumber ("activeFrame", frameIdValue threadState.ActiveMethodState)
            writer.WritePropertyName "activeFrameSummary"

            // Only the active frame is rendered here, so ask for only its offset. `/state` is
            // emitted on every step, and building the whole stack's map each time would make
            // single-stepping a deeply recursive guest cost time quadratic in its depth.
            writeFrameSummary
                writer
                state
                (GuestLocation.activeAttributionOffset threadState)
                threadState.ActiveMethodState
                threadState.ActiveMethodState
                threadState.MethodState

        writer.WriteEndObject ()

    let private writeValueArray<'a>
        (writer : Utf8JsonWriter)
        (name : string)
        (values : seq<'a>)
        (writeOne : Utf8JsonWriter -> 'a -> unit)
        : unit
        =
        writer.WritePropertyName name
        writer.WriteStartArray ()

        for value in values do
            writeOne writer value

        writer.WriteEndArray ()

    let private writeEvalStackValue
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (value : EvalStackValue)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteString ("value", string value)
        writer.WritePropertyName "structured"
        DebuggerValueJson.writeEvalStackValue writer context value

        match value with
        | EvalStackValue.ObjectRef address -> writer.WriteNumber ("objectAddress", heapAddressValue address)
        | EvalStackValue.NullObjectRef -> writer.WriteNull "objectAddress"
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.NativeInt _
        | EvalStackValue.Float _
        | EvalStackValue.ManagedPointer _
        | EvalStackValue.UserDefinedValueType _ -> ()

        writer.WriteEndObject ()

    let private writeCliType (writer : Utf8JsonWriter) (context : DebuggerValueContext) (value : CliType) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("value", string value)
        writer.WritePropertyName "structured"
        DebuggerValueJson.writeCliType writer context value

        match value with
        | CliType.ObjectRef address -> writeOptionalHeapAddress writer "objectAddress" address
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _
        | CliType.RuntimePointer _
        | CliType.ValueType _ -> ()

        writer.WriteEndObject ()

    let private writeFrameDetails
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (sourceIlOffset : int)
        (activeFrame : FrameId)
        (frameId : FrameId)
        (frame : MethodState)
        : unit
        =
        let context = DebuggerValueJson.ofState state
        writer.WriteStartObject ()
        writeFrameProperties writer state sourceIlOffset true activeFrame frameId frame

        writeValueArray
            writer
            "evalStack"
            frame.EvaluationStack.Values
            (fun writer value -> writeEvalStackValue writer context value)

        writeValueArray writer "arguments" frame.Arguments (fun writer value -> writeCliType writer context value)
        writeValueArray writer "locals" frame.LocalVariables (fun writer value -> writeCliType writer context value)
        writer.WriteEndObject ()

    let private writeRunOutcome (writer : Utf8JsonWriter) (outcome : RunOutcome) : unit =
        writer.WritePropertyName "outcome"
        writer.WriteStartObject ()

        match outcome with
        | RunOutcome.NormalExit (state, thread) ->
            writer.WriteString ("kind", "normalExit")
            writer.WriteNumber ("thread", threadIdValue thread)
            writer.WriteNumber ("exitCode", state.LatchedExitCode)
        | RunOutcome.ProcessExit (state, thread) ->
            writer.WriteString ("kind", "processExit")
            writer.WriteNumber ("thread", threadIdValue thread)
            writer.WriteNumber ("exitCode", state.LatchedExitCode)
        | RunOutcome.Aborted (_state, thread, fatal) ->
            writer.WriteString ("kind", "aborted")
            writer.WriteNumber ("thread", threadIdValue thread)
            writer.WriteString ("code", sprintf "%O" fatal.Code)
            writer.WriteNumber ("hresult", FatalErrorCode.toHResult fatal.Code)

            match fatal.Message with
            | Some m -> writer.WriteString ("message", m)
            | None -> writer.WriteNull "message"
        | RunOutcome.SignalTerminated (state, signal) ->
            // The signo is read under the platform the guest simulated, since
            // that is what its own shell would have reported.
            let signo =
                Signal.toRawSignoUnder (SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform) signal

            writer.WriteString ("kind", "signalTerminated")
            writer.WriteString ("signal", sprintf "%O" signal)
            writer.WriteNumber ("signo", signo)
            writer.WriteNumber ("exitCode", 128 + signo)
        | RunOutcome.GuestUnhandledException (_, thread, exn) ->
            writer.WriteString ("kind", "guestUnhandledException")
            writer.WriteNumber ("thread", threadIdValue thread)
            writer.WriteString ("exceptionObject", string exn.ExceptionObject)
            writer.WriteNumber ("exceptionObjectAddress", heapAddressValue exn.ExceptionObject)

        writer.WriteEndObject ()

    type private SessionState =
        | Running of Program.PreparedProgram * stepsExecuted : int64
        | Finished of RunOutcome * stepsExecuted : int64
        | Deadlocked of Program.PreparedProgram * stuckThreads : string * stepsExecuted : int64

    type private DebugEvent =
        {
            StepNumber : int64
            Kind : string
            Thread : int option
            Detail : string
            BlockedOnClassInitThread : int option
            Effect : StepEffect
        }

    let private sessionState (session : SessionState) : IlMachineState =
        match session with
        | SessionState.Running (prepared, _) -> prepared.State
        | SessionState.Finished (RunOutcome.NormalExit (state, _), _)
        | SessionState.Finished (RunOutcome.ProcessExit (state, _), _)
        | SessionState.Finished (RunOutcome.Aborted (state, _, _), _)
        | SessionState.Finished (RunOutcome.SignalTerminated (state, _), _)
        | SessionState.Finished (RunOutcome.GuestUnhandledException (state, _, _), _) -> state
        | SessionState.Deadlocked (prepared, _, _) -> prepared.State

    let private prepareSession
        (loggerFactory : ILoggerFactory)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (kernelConfig : KernelConfig)
        (pctSeed : uint64 option)
        (argv : string list)
        : SessionState
        =
        use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

        match
            Program.prepare
                loggerFactory
                (Some dllPath)
                fileStream
                {
                    Guest =
                        { GuestConfig.Default dotnetRuntimeDirs with
                            Kernel = kernelConfig
                            Argv = argv
                            AssemblyPath = Some dllPath
                            AppContext = HostRuntimeConfig.forAssembly dllPath
                        }
                    PctSeed = pctSeed
                }
        with
        | Program.ProgramStartResult.Ready prepared -> SessionState.Running (prepared, 0L)
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> SessionState.Finished (outcome, 0L)

    let private eventOfStepOutcome (stepNumber : int64) (outcome : Program.ProgramStepOutcome) : DebugEvent =
        match outcome with
        | Program.ProgramStepOutcome.InstructionStepped (_, thread, whatWeDid, effect) ->
            let blockedOnClassInitThread =
                match whatWeDid with
                | WhatWeDid.BlockedOnClassInit blocker -> Some (threadIdValue blocker)
                | WhatWeDid.Executed
                | WhatWeDid.Aborted _
                | WhatWeDid.UnhandledException _
                | WhatWeDid.VoluntaryYield _
                | WhatWeDid.SuspendedForClassInit
                | WhatWeDid.SuspendedForManagedCall
                | WhatWeDid.ThrowingTypeInitializationException -> None

            {
                StepNumber = stepNumber
                Kind = "instruction"
                Thread = Some (threadIdValue thread)
                Detail = string whatWeDid
                BlockedOnClassInitThread = blockedOnClassInitThread
                Effect = effect
            }
        | Program.ProgramStepOutcome.WorkerTerminated (_, thread) ->
            {
                StepNumber = stepNumber
                Kind = "workerTerminated"
                Thread = Some (threadIdValue thread)
                Detail = "thread terminated"
                BlockedOnClassInitThread = None
                Effect = StepEffect.NoEffect
            }
        | Program.ProgramStepOutcome.Completed outcome ->
            let detail =
                match outcome with
                | RunOutcome.NormalExit _ -> "normal exit"
                | RunOutcome.ProcessExit _ -> "process exit"
                | RunOutcome.Aborted (_, _, fatal) -> sprintf "aborted (%O)" fatal.Code
                | RunOutcome.SignalTerminated _ -> "signal terminated"
                | RunOutcome.GuestUnhandledException _ -> "guest unhandled exception"

            {
                StepNumber = stepNumber
                Kind = "completed"
                Thread = None
                Detail = detail
                BlockedOnClassInitThread = None
                Effect = StepEffect.NoEffect
            }
        | Program.ProgramStepOutcome.Deadlocked (_, stuck) ->
            {
                StepNumber = stepNumber
                Kind = "deadlocked"
                Thread = None
                Detail = stuck
                BlockedOnClassInitThread = None
                Effect = StepEffect.NoEffect
            }

    let private stepSession
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (session : SessionState)
        : SessionState * DebugEvent * bool
        =
        match session with
        | SessionState.Running (prepared, steps) ->
            let outcome = Program.stepPrepared loggerFactory logger prepared

            match outcome with
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) ->
                let steps = steps + 1L
                SessionState.Running (prepared, steps), eventOfStepOutcome steps outcome, true
            | Program.ProgramStepOutcome.Completed runOutcome ->
                let steps = steps + 1L
                SessionState.Finished (runOutcome, steps), eventOfStepOutcome steps outcome, true
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) ->
                SessionState.Deadlocked (prepared, stuck, steps), eventOfStepOutcome steps outcome, false
        | SessionState.Finished (_, steps) ->
            session,
            {
                StepNumber = steps
                Kind = "alreadyFinished"
                Thread = None
                Detail = "program has already finished"
                BlockedOnClassInitThread = None
                Effect = StepEffect.NoEffect
            },
            false
        | SessionState.Deadlocked (_, stuck, steps) ->
            session,
            {
                StepNumber = steps
                Kind = "alreadyDeadlocked"
                Thread = None
                Detail = stuck
                BlockedOnClassInitThread = None
                Effect = StepEffect.NoEffect
            },
            false

    let private streamName (role : FileDescriptorRole) : string =
        match role with
        | FileDescriptorRole.StandardOutput -> "stdout"
        | FileDescriptorRole.StandardError -> "stderr"
        // `SystemNative_Write` refuses stdin before logging anything, so this names a state the
        // interpreter should never reach; it is reported rather than raised, as elsewhere here.
        | FileDescriptorRole.StandardInput -> "stdin"

    /// The bytes are base64 because they are whatever the guest wrote, which need not be UTF-8.
    let private writeOutputEntry
        (writer : Utf8JsonWriter)
        (role : FileDescriptorRole)
        (bytes : ImmutableArray<byte>)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteString ("stream", streamName role)
        writer.WriteBase64String ("bytesBase64", bytes.AsSpan ())
        writer.WriteEndObject ()

    /// What happened in a step, as opposed to when (its number) or what it wrote (its output).
    let private writeEventProperties (writer : Utf8JsonWriter) (event : DebugEvent) : unit =
        writer.WriteString ("kind", event.Kind)
        writeOptionalInt writer "thread" event.Thread
        writer.WriteString ("detail", event.Detail)

        match event.BlockedOnClassInitThread with
        | Some blocker -> writer.WriteNumber ("blockedOnClassInitThread", blocker)
        | None -> ()

    let private writeEvent (writer : Utf8JsonWriter) (event : DebugEvent) : unit =
        writer.WriteStartObject ()
        writer.WriteNumber ("step", event.StepNumber)
        writeEventProperties writer event

        writer.WritePropertyName "output"

        match event.Effect with
        | StepEffect.NoEffect -> writer.WriteNullValue ()
        | StepEffect.WroteToFd (role, bytes) -> writeOutputEntry writer role bytes

        writer.WriteEndObject ()

    let private writeSessionSummary (writer : Utf8JsonWriter) (session : SessionState) : unit =
        let state = sessionState session

        writer.WritePropertyName "session"
        writer.WriteStartObject ()

        match session with
        | SessionState.Running (prepared, steps) ->
            writer.WriteString ("status", "running")
            writer.WriteNumber ("stepsExecuted", steps)
            writer.WriteNumber ("entryThread", threadIdValue prepared.EntryThread)
            writer.WriteNumber ("lastRan", threadIdValue prepared.LastRan)
        | SessionState.Finished (outcome, steps) ->
            writer.WriteString ("status", "finished")
            writer.WriteNumber ("stepsExecuted", steps)
            writeRunOutcome writer outcome
        | SessionState.Deadlocked (prepared, stuck, steps) ->
            writer.WriteString ("status", "deadlocked")
            writer.WriteNumber ("stepsExecuted", steps)
            writer.WriteNumber ("entryThread", threadIdValue prepared.EntryThread)
            writer.WriteNumber ("lastRan", threadIdValue prepared.LastRan)
            writer.WriteString ("stuckThreads", stuck)

        writer.WritePropertyName "heap"
        writer.WriteStartObject ()
        writer.WriteNumber ("nonArrayObjects", HeapObserver.nonArrayObjectCount state.ManagedHeap)
        writer.WriteNumber ("arrays", HeapObserver.arrayCount state.ManagedHeap)
        writer.WriteNumber ("stringContents", HeapObserver.stringContentCount state.ManagedHeap)
        writer.WriteEndObject ()

        writeValueArray
            writer
            "loadedAssemblies"
            state._LoadedAssemblies.DefinitionNames
            (fun writer assemblyName -> writer.WriteStringValue assemblyName)

        writeValueArray
            writer
            "threads"
            (state.ThreadState |> Map.toSeq)
            (fun writer (threadId, threadState) -> writeThreadSummary writer state threadId threadState)

        writer.WriteEndObject ()

    let private writeStateResponse (writer : Utf8JsonWriter) (session : SessionState) : unit =
        writer.WriteStartObject ()
        writeSessionSummary writer session
        writer.WriteEndObject ()

    /// Everything the guest has written so far, in write order across both streams.
    let private writeOutputResponse (writer : Utf8JsonWriter) (session : SessionState) : unit =
        let state = sessionState session
        writer.WriteStartObject ()

        writeValueArray
            writer
            "entries"
            state.Kernel.OutputLog
            (fun writer entry -> writeOutputEntry writer entry.Role entry.Bytes)

        writer.WriteEndObject ()

    let private writeThreadResponse (writer : Utf8JsonWriter) (session : SessionState) (threadId : ThreadId) : unit =
        let state = sessionState session

        match state.ThreadState |> Map.tryFind threadId with
        | None ->
            writer.WriteStartObject ()
            writer.WriteString ("error", $"thread %d{threadIdValue threadId} does not exist")
            writer.WriteEndObject ()
        | Some threadState ->
            writer.WriteStartObject ()
            writer.WriteNumber ("id", threadIdValue threadId)
            writer.WritePropertyName "status"
            writeThreadStatus writer (state.Kernel.Tasks |> Map.tryFind threadId) threadState.Status

            if ThreadStatus.hasNoActiveFrame threadState.Status then
                writer.WriteNull "activeAssembly"
                writer.WriteNull "activeFrame"
            else
                writer.WriteString ("activeAssembly", threadState.ActiveAssemblyFullName)
                writer.WriteNumber ("activeFrame", frameIdValue threadState.ActiveMethodState)

            let offsets = GuestLocation.attributionOffsets threadState

            writeValueArray
                writer
                "frames"
                (threadState.MethodStates |> Map.toSeq)
                (fun writer (frameId, frame) ->
                    writeFrameDetails
                        writer
                        state
                        (Map.find frameId offsets)
                        threadState.ActiveMethodState
                        frameId
                        frame
                )

            writer.WriteEndObject ()

    let private writeMethodCount (writer : Utf8JsonWriter) (methodName : string, count : int) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("method", methodName)
        writer.WriteNumber ("count", count)
        writer.WriteEndObject ()

    let private methodCounts (frames : (FrameId * MethodState) array) (limit : int) : (string * int) array =
        let counts = Dictionary<string, int> ()

        for _, frame in frames do
            let methodName = string frame.ExecutingMethod

            match counts.TryGetValue methodName with
            | true, count -> counts.[methodName] <- count + 1
            | false, _ -> counts.[methodName] <- 1

        counts
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Seq.sortWith (fun (methodA, countA) (methodB, countB) ->
            match compare countB countA with
            | 0 -> StringComparer.Ordinal.Compare (methodA, methodB)
            | ordering -> ordering
        )
        |> Seq.truncate limit
        |> Seq.toArray

    let private edgeFrameCount (frames : 'a array) (edgeFrames : int) : int = min edgeFrames frames.Length

    let private qualifiedTypeNameForMethod
        (assembly : DumpedAssembly)
        (method : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : string
        =
        match method.TryDeclaringType with
        // A `Reflection.Emit` method is owned by a synthetic per-module class with no TypeDef row,
        // and real .NET renders such a frame with no type name at all (`at Thrower(Int32)`).
        // `MethodOwner.describe` renders that in a form no type name could be confused with.
        | None -> MethodOwner.describe method.Owner
        | Some declaringType ->

        match assembly.TypeDefs.TryGetValue declaringType.Definition.Get with
        | true, typeInfo -> IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo
        | false, _ -> string declaringType

    let private writeThreadStackSummaryResponse
        (writer : Utf8JsonWriter)
        (session : SessionState)
        (threadId : ThreadId)
        (edgeFrames : int)
        (topMethods : int)
        : unit
        =
        let state = sessionState session

        match state.ThreadState |> Map.tryFind threadId with
        | None ->
            writer.WriteStartObject ()
            writer.WriteString ("error", $"thread %d{threadIdValue threadId} does not exist")
            writer.WriteEndObject ()
        | Some threadState ->
            let frames = threadState.MethodStates |> Map.toArray
            let edgeFrames = edgeFrameCount frames edgeFrames
            let firstFrames = frames |> Array.truncate edgeFrames
            let lastFrames = frames |> Array.skip (frames.Length - edgeFrames)
            let methodCounts = methodCounts frames topMethods
            let offsets = GuestLocation.attributionOffsets threadState

            writer.WriteStartObject ()
            writer.WriteNumber ("id", threadIdValue threadId)
            writer.WritePropertyName "status"
            writeThreadStatus writer (state.Kernel.Tasks |> Map.tryFind threadId) threadState.Status

            if ThreadStatus.hasNoActiveFrame threadState.Status then
                writer.WriteNull "activeAssembly"
                writer.WriteNull "activeFrame"
                writer.WriteNumber ("frameCount", frames.Length)
                writer.WriteNull "activeFrameSummary"
            else
                writer.WriteString ("activeAssembly", threadState.ActiveAssemblyFullName)
                writer.WriteNumber ("activeFrame", frameIdValue threadState.ActiveMethodState)
                writer.WriteNumber ("frameCount", frames.Length)
                writer.WritePropertyName "activeFrameSummary"

                writeFrameSummary
                    writer
                    state
                    (Map.find threadState.ActiveMethodState offsets)
                    threadState.ActiveMethodState
                    threadState.ActiveMethodState
                    threadState.MethodState

            writeValueArray writer "topMethods" methodCounts writeMethodCount

            writeValueArray
                writer
                "firstFrames"
                firstFrames
                (fun writer (frameId, frame) ->
                    writeFrameSummary
                        writer
                        state
                        (Map.find frameId offsets)
                        threadState.ActiveMethodState
                        frameId
                        frame
                )

            writeValueArray
                writer
                "lastFrames"
                lastFrames
                (fun writer (frameId, frame) ->
                    writeFrameSummary
                        writer
                        state
                        (Map.find frameId offsets)
                        threadState.ActiveMethodState
                        frameId
                        frame
                )

            writer.WriteEndObject ()

    /// `AllConcreteTypes.describe`'s rendering, e.g. `System.Object#3 [System.Private.CoreLib]`:
    /// the handle and assembly tell apart two types of the same name. It never throws, so a handle
    /// the registry cannot name still renders, as a placeholder that says so.
    let private typeDescription (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes handle

    let private writeLocalType
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (index : int, localType : ConcreteTypeHandle)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteNumber ("index", index)
        writer.WriteString ("type", string localType)
        writer.WriteString ("typeDescription", typeDescription state localType)
        writer.WriteEndObject ()

    let private writeInstructionLine
        (writer : Utf8JsonWriter)
        (assembly : DumpedAssembly)
        (scope : GenericScope)
        (activeIlOffset : int)
        (ilOp : IlOp, offset : int)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteNumber ("offset", offset)
        writer.WriteString ("op", string ilOp)
        writer.WriteString ("text", (IlFormatting.formatIlOp assembly scope ilOp offset).TrimStart ())
        let active = offset = activeIlOffset
        writer.WriteBoolean ("active", active)
        writer.WriteEndObject ()

    let private instructionWindow
        (activeIlOffset : int)
        (context : int option)
        (instructions : (IlOp * int) array)
        : (IlOp * int) array * bool * bool
        =
        match context with
        | None -> instructions, false, false
        | Some context ->
            match instructions |> Array.tryFindIndex (fun (_, offset) -> offset = activeIlOffset) with
            | None -> instructions, false, false
            | Some activeIndex ->
                let firstIndex = max 0 (activeIndex - context)
                let lastIndexInclusive = min (instructions.Length - 1) (activeIndex + context)
                let count = lastIndexInclusive - firstIndex + 1

                instructions |> Array.skip firstIndex |> Array.truncate count,
                firstIndex > 0,
                lastIndexInclusive < instructions.Length - 1

    let private writeActiveMethodIlResponse
        (writer : Utf8JsonWriter)
        (session : SessionState)
        (threadId : ThreadId)
        (context : int option)
        : unit
        =
        let state = sessionState session

        match state.ThreadState |> Map.tryFind threadId with
        | None ->
            writer.WriteStartObject ()
            writer.WriteString ("error", $"thread %d{threadIdValue threadId} does not exist")
            writer.WriteEndObject ()
        | Some threadState when ThreadStatus.hasNoActiveFrame threadState.Status ->
            // No frame has been pushed yet (pre-`Start`, or a kernel-owned
            // Parked dispatcher); there is no active method whose IL we
            // could disassemble. Report this rather than dereferencing the
            // sentinel ActiveMethodState.
            writer.WriteStartObject ()
            writer.WriteNumber ("thread", threadIdValue threadId)
            writer.WriteString ("error", $"thread %d{threadIdValue threadId} has no active frame")
            writer.WriteEndObject ()
        | Some threadState ->
            let frameId = threadState.ActiveMethodState
            let frame = threadState.MethodState

            let assembly =
                state._LoadedAssemblies.ByDefinitionName frame.ExecutingMethod.DeclaringAssemblyFullName

            let qualifiedTypeName = qualifiedTypeNameForMethod assembly frame.ExecutingMethod

            // The executing method has been concretised, so its own generic parameters no longer
            // carry their declared names; recover them from the metadata flavour of the same
            // method. If it isn't in the index, the indices render positionally.
            // A synthesised frame has no metadata flavour to recover names from, so it renders
            // positionally too.
            let scope =
                match frame.ExecutingMethod.TryMetadata with
                | None -> GenericScope.unknown
                | Some facts ->
                    match assembly.Methods.TryGetValue facts.Handle with
                    | true, method -> GenericScope.ofMethod method
                    | false, _ -> GenericScope.unknown

            writer.WriteStartObject ()
            writer.WriteNumber ("thread", threadIdValue threadId)
            writer.WriteNumber ("frame", frameIdValue frameId)
            writer.WriteString ("method", string frame.ExecutingMethod)
            writer.WriteString ("declaringType", qualifiedTypeName)
            writer.WriteNumber ("activeIlOffset", frame.IlOpIndex)

            match MethodInfo.tryIlBody frame.ExecutingMethod with
            | None ->
                writer.WriteBoolean ("hasBody", false)
                writer.WriteNull "localsInit"
                writeValueArray writer "locals" [] (fun writer local -> writeLocalType writer state local)

                writeValueArray
                    writer
                    "instructions"
                    []
                    (fun writer instruction -> writeInstructionLine writer assembly scope frame.IlOpIndex instruction)

                writer.WriteBoolean ("truncatedBefore", false)
                writer.WriteBoolean ("truncatedAfter", false)
            | Some instructions ->
                writer.WriteBoolean ("hasBody", true)
                writer.WriteBoolean ("localsInit", instructions.LocalsInit)

                let locals =
                    match instructions.LocalVars with
                    | None -> Array.empty
                    | Some locals -> locals |> Seq.mapi (fun i localType -> i, localType) |> Seq.toArray

                writeValueArray writer "locals" locals (fun writer local -> writeLocalType writer state local)

                let instructionArray = instructions.Instructions |> List.toArray

                let instructionWindow, truncatedBefore, truncatedAfter =
                    instructionWindow frame.IlOpIndex context instructionArray

                writeValueArray
                    writer
                    "instructions"
                    instructionWindow
                    (fun writer instruction -> writeInstructionLine writer assembly scope frame.IlOpIndex instruction)

                writer.WriteBoolean ("truncatedBefore", truncatedBefore)
                writer.WriteBoolean ("truncatedAfter", truncatedAfter)

            writer.WriteEndObject ()

    let private hasThread (session : SessionState) (threadId : ThreadId) : bool =
        let state = sessionState session
        state.ThreadState |> Map.containsKey threadId

    /// The heap objects an object refers to directly, as `references`: each address once, in
    /// ascending order.
    let private writeReferences (writer : Utf8JsonWriter) (references : ManagedHeapAddress list) : unit =
        writeValueArray
            writer
            "references"
            (references |> List.distinct |> List.sort)
            (fun writer address -> writer.WriteNumberValue (heapAddressValue address))

    /// How many UTF-16 code units of a string the heap listing shows.
    let internal stringPreviewLength : int = 256

    /// Whether `text` can be written as a JSON string: `Utf8JsonWriter` refuses a lone surrogate,
    /// which a guest string is free to contain.
    let private isWellFormedUtf16 (text : string) : bool =
        let mutable ok = true
        let mutable i = 0

        while ok && i < text.Length do
            if Char.IsHighSurrogate text.[i] then
                if i + 1 < text.Length && Char.IsLowSurrogate text.[i + 1] then
                    i <- i + 2
                else
                    ok <- false
            elif Char.IsLowSurrogate text.[i] then
                ok <- false
            else
                i <- i + 1

        ok

    /// The heap listing's preview of a string, and whether it is shorter than the string. The
    /// preview is the string's first `stringPreviewLength` code units, one fewer if the last of
    /// those is a high surrogate (so a pair is never split); it is `None` if that prefix holds a
    /// lone surrogate, which `Utf8JsonWriter` cannot write.
    let internal stringPreview (text : string) : string option * bool =
        let truncated = text.Length > stringPreviewLength

        let prefix =
            if not truncated then
                text
            elif Char.IsHighSurrogate text.[stringPreviewLength - 1] then
                text.Substring (0, stringPreviewLength - 1)
            else
                text.Substring (0, stringPreviewLength)

        (if isWellFormedUtf16 prefix then Some prefix else None), truncated

    let private writeHeapListingEntry
        (writer : Utf8JsonWriter)
        (state : IlMachineState)
        (address : ManagedHeapAddress)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteNumber ("address", heapAddressValue address)

        let concreteType =
            match HeapObserver.tryGetNonArrayObject address state.ManagedHeap with
            | Some object ->
                writer.WriteString ("kind", "object")
                writer.WriteNull "length"
                object.ConcreteType
            | None ->
                match HeapObserver.tryGetArray address state.ManagedHeap with
                | Some array ->
                    writer.WriteString ("kind", "array")
                    writer.WriteNumber ("length", array.Shape.Length)
                    array.Shape.ConcreteType
                | None -> failwith $"heap listing: live address %d{heapAddressValue address} has no payload"

        writer.WriteString ("type", typeDescription state concreteType)

        match HeapObserver.getStringContents address state.ManagedHeap with
        | None ->
            writer.WriteNull "string"
            writer.WriteNull "stringLength"
            writer.WriteBoolean ("stringTruncated", false)
        | Some text ->
            let preview, truncated = stringPreview text

            match preview with
            | Some preview -> writer.WriteString ("string", preview)
            | None -> writer.WriteNull "string"

            writer.WriteNumber ("stringLength", text.Length)
            writer.WriteBoolean ("stringTruncated", truncated)

        writer.WriteEndObject ()

    /// One page of the live heap in ascending address order: up to `limit` objects with addresses
    /// above `after`. `nextAfter` is the `after` that fetches the next page, or null on the last.
    /// Addresses are never reused, so paging stays consistent while the guest allocates.
    let private writeHeapListingResponse
        (writer : Utf8JsonWriter)
        (session : SessionState)
        (after : ManagedHeapAddress option)
        (limit : int)
        : unit
        =
        let state = sessionState session
        let live = HeapObserver.liveAddresses state.ManagedHeap

        let candidates =
            match after with
            | None -> live :> seq<ManagedHeapAddress>
            | Some after -> live |> Seq.filter (fun address -> address > after)

        let page = candidates |> Seq.truncate (limit + 1) |> Seq.toList
        let hasMore = page.Length > limit
        let page = page |> List.truncate limit

        writer.WriteStartObject ()
        writer.WriteNumber ("liveCount", live.Count)
        writer.WriteNumber ("limit", limit)
        writeValueArray writer "objects" page (fun writer address -> writeHeapListingEntry writer state address)

        match hasMore, List.tryLast page with
        | true, Some last -> writer.WriteNumber ("nextAfter", heapAddressValue last)
        | true, None
        | false, _ -> writer.WriteNull "nextAfter"

        writer.WriteEndObject ()

    let private writeTypeInitState (writer : Utf8JsonWriter) (initState : TypeInitState option) : unit =
        writer.WritePropertyName "initState"

        match initState with
        | None -> writer.WriteNullValue ()
        | Some initState ->
            writer.WriteStartObject ()

            match initState with
            | TypeInitState.Initialized -> writer.WriteString ("kind", "initialized")
            | TypeInitState.InProgress thread ->
                writer.WriteString ("kind", "inProgress")
                writer.WriteNumber ("thread", threadIdValue thread)
            | TypeInitState.Failed (exceptionAddress, _) ->
                writer.WriteString ("kind", "failed")
                writer.WriteNumber ("exception", heapAddressValue exceptionAddress)

            writer.WriteEndObject ()

    /// The static fields of every type that has begun initialisation or has a written static
    /// slot. Each field lists its written slots; a field with none holds its type's zero in every
    /// slot, except an RVA-backed one, whose contents live in the PE image and are not reported.
    /// `const` fields have no storage and are omitted.
    let private writeStaticsResponse (writer : Utf8JsonWriter) (session : SessionState) : unit =
        let state = sessionState session
        let context = DebuggerValueJson.ofState state

        let writtenByType =
            StaticStorageObserver.writtenSlots state.Statics
            |> List.groupBy (fun (_, ty, _, _) -> ty)
            |> Map.ofList

        let types =
            Set.union
                (state.TypeInitTable |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq)
                (writtenByType |> Map.keys |> Set.ofSeq)

        let writeSlots (slots : (StaticOwner * CliType) list) : unit =
            writeValueArray
                writer
                "slots"
                slots
                (fun writer (owner, value) ->
                    writer.WriteStartObject ()
                    writer.WritePropertyName "owner"
                    writer.WriteStartObject ()

                    match owner with
                    | StaticOwner.Shared -> writer.WriteString ("kind", "shared")
                    | StaticOwner.OwnedBy thread ->
                        writer.WriteString ("kind", "thread")
                        writer.WriteNumber ("thread", threadIdValue thread)

                    writer.WriteEndObject ()
                    writer.WritePropertyName "value"
                    DebuggerValueJson.writeCliType writer context value
                    writer.WriteEndObject ()
                )

        writer.WriteStartObject ()

        writeValueArray
            writer
            "types"
            types
            (fun writer ty ->
                let written =
                    writtenByType
                    |> Map.tryFind ty
                    |> Option.defaultValue []
                    |> List.map (fun (owner, _, field, value) -> field, (owner, value))

                let slotsOf (field : ComparableFieldDefinitionHandle) : (StaticOwner * CliType) list =
                    written |> List.filter (fun (f, _) -> f = field) |> List.map snd

                let declared =
                    match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes ty with
                    | None -> []
                    | Some (_, typeInfo) ->
                        typeInfo.Fields
                        |> List.filter (fun field ->
                            field.IsStatic
                            && not (field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal)
                        )

                let declaredHandles =
                    declared
                    |> List.map (fun field -> ComparableFieldDefinitionHandle.Make field.Handle)
                    |> Set.ofList

                // A written slot for a field the type does not declare would be an interpreter
                // bug; it is reported rather than hidden.
                let undeclared =
                    written
                    |> List.map fst
                    |> List.distinct
                    |> List.filter (fun field -> not (declaredHandles.Contains field))

                writer.WriteStartObject ()
                writer.WriteString ("type", string ty)
                writer.WriteString ("typeDescription", typeDescription state ty)
                writeTypeInitState writer (TypeInitTable.tryGet ty state.TypeInitTable)
                writer.WriteStartArray "fields"

                for field in declared do
                    let handle = ComparableFieldDefinitionHandle.Make field.Handle
                    writer.WriteStartObject ()
                    writer.WriteString ("name", field.Name)
                    writer.WriteString ("token", string handle)

                    writer.WriteString (
                        "storage",
                        if field.HasFieldRVA then "rva"
                        elif field.IsThreadStatic then "threadStatic"
                        else "shared"
                    )

                    writeSlots (slotsOf handle)
                    writer.WriteEndObject ()

                for handle in undeclared do
                    writer.WriteStartObject ()
                    writer.WriteNull "name"
                    writer.WriteString ("token", string handle)
                    writer.WriteNull "storage"
                    writeSlots (slotsOf handle)
                    writer.WriteEndObject ()

                writer.WriteEndArray ()
                writer.WriteEndObject ()
            )

        writer.WriteEndObject ()

    let private writeHeapObjectResponse
        (writer : Utf8JsonWriter)
        (session : SessionState)
        (address : ManagedHeapAddress)
        : unit
        =
        let state = sessionState session
        writer.WriteStartObject ()
        writer.WriteNumber ("address", heapAddressValue address)

        let context = DebuggerValueJson.ofState state

        match HeapObserver.tryGetNonArrayObject address state.ManagedHeap with
        | Some object ->
            writer.WriteString ("kind", "object")
            writer.WriteString ("concreteType", string object.ConcreteType)
            writer.WriteString ("typeDescription", typeDescription state object.ConcreteType)
            writer.WriteString ("contents", string object.Contents)
            DebuggerValueJson.writeValueTypeFields writer context object.Contents
            writeReferences writer (DebuggerValueJson.referencesOfValueType object.Contents)
            writeOptionalString writer "string" (HeapObserver.getStringContents address state.ManagedHeap)
            writer.WriteString ("syncBlock", string (HeapObserver.getSyncBlock address state.ManagedHeap))
        | None ->
            match HeapObserver.tryGetArray address state.ManagedHeap with
            | Some array ->
                writer.WriteString ("kind", "array")
                writer.WriteString ("concreteType", string array.Shape.ConcreteType)
                writer.WriteString ("typeDescription", typeDescription state array.Shape.ConcreteType)
                writer.WriteNumber ("length", array.Shape.Length)

                writeValueArray writer "elements" array.Elements (fun writer value -> writeCliType writer context value)

                writeReferences
                    writer
                    (array.Elements
                     |> Seq.collect DebuggerValueJson.referencesOfCliType
                     |> Seq.toList)
                // Arrays carry an object header exactly like any other heap object, so a
                // `lock (array)` is visible here too.
                writer.WriteString ("syncBlock", string (HeapObserver.getSyncBlock address state.ManagedHeap))
            | None ->
                writer.WriteString ("kind", "missing")
                writer.WriteString ("error", $"heap address %d{heapAddressValue address} does not exist")

        writer.WriteEndObject ()

    let private hasHeapAddress (session : SessionState) (address : ManagedHeapAddress) : bool =
        let state = sessionState session

        HeapObserver.isLive address state.ManagedHeap

    type private DebuggerHttpResponse =
        {
            StatusCode : int
            ContentType : string
            Body : byte[]
            ExtraHeaders : (string * string) list
        }

    type private HandlerResult =
        {
            Response : DebuggerHttpResponse
            StopAfterResponse : bool
            ReleaseActiveStepRequestAfterResponse : bool
        }

    let private jsonResponseWith
        (options : JsonWriterOptions)
        (statusCode : int)
        (write : Utf8JsonWriter -> unit)
        : DebuggerHttpResponse
        =
        use stream = new MemoryStream ()
        use writer = new Utf8JsonWriter (stream, options)
        write writer
        writer.Flush ()

        {
            StatusCode = statusCode
            ContentType = "application/json; charset=utf-8"
            Body = stream.ToArray ()
            ExtraHeaders = []
        }

    let private jsonResponse (statusCode : int) (write : Utf8JsonWriter -> unit) : DebuggerHttpResponse =
        jsonResponseWith (JsonWriterOptions (Indented = true)) statusCode write

    let private textResponse (statusCode : int) (text : string) : DebuggerHttpResponse =
        {
            StatusCode = statusCode
            ContentType = "text/plain; charset=utf-8"
            Body = Encoding.UTF8.GetBytes text
            ExtraHeaders = []
        }

    let private writeFailureProperties (writer : Utf8JsonWriter) (operation : string) (ex : exn) : unit =
        writer.WriteString ("operation", operation)
        writer.WriteString ("error", ex.Message)
        writer.WriteString ("exceptionType", ex.GetType().FullName)

    let private requestFailureResponse (operation : string) (ex : exn) (session : SessionState) : DebuggerHttpResponse =
        jsonResponse
            500
            (fun writer ->
                writer.WriteStartObject ()
                writeFailureProperties writer operation ex
                writeSessionSummary writer session
                writer.WriteEndObject ()
            )

    let private unauthorisedResponse : DebuggerHttpResponse =
        { textResponse 401 "Unauthorized" with
            ExtraHeaders = [ "WWW-Authenticate", "Bearer" ]
        }

    let private writeResponse (context : HttpContext) (response : DebuggerHttpResponse) : Task =
        task {
            context.Response.StatusCode <- response.StatusCode
            context.Response.ContentType <- response.ContentType
            context.Response.ContentLength <- Nullable<int64> (int64 response.Body.Length)
            context.Response.Headers.[HeaderNames.CacheControl] <- StringValues "no-store"

            for name, value in response.ExtraHeaders do
                context.Response.Headers.[name] <- StringValues value

            do! context.Response.Body.WriteAsync (response.Body, 0, response.Body.Length)
        }

    let private generateBearerToken () : string =
        let bytes = RandomNumberGenerator.GetBytes 32
        Convert.ToHexString bytes

    let private fixedTimeEquals (expected : string) (actual : string) : bool =
        // The generated bearer token has a fixed-length hex encoding, so rejecting
        // different-length headers before the fixed-time loop does not leak which
        // prefix, if any, matched.
        if expected.Length <> actual.Length then
            false
        else
            let mutable diff = 0

            for i = 0 to expected.Length - 1 do
                diff <- diff ||| (int expected.[i] ^^^ int actual.[i])

            diff = 0

    let private isAuthorised (token : string) (context : HttpContext) : bool =
        let header = context.Request.Headers.Authorization.ToString ()
        fixedTimeEquals $"Bearer %s{token}" header

    let private parsePositiveInt (name : string) (defaultValue : int) (maximum : int) (query : IQueryCollection) : int =
        let raw = query.[name].ToString ()

        if String.IsNullOrWhiteSpace raw then
            defaultValue
        else
            match Int32.TryParse raw with
            | true, value when value > 0 -> min value maximum
            | _ -> defaultValue

    let private parseOptionalPositiveInt (name : string) (maximum : int) (query : IQueryCollection) : int option =
        let raw = query.[name].ToString ()

        if String.IsNullOrWhiteSpace raw then
            None
        else
            match Int32.TryParse raw with
            | true, value when value > 0 -> Some (min value maximum)
            | _ -> None

    type private RunStepsResult =
        {
            Session : SessionState
            StepsRun : int
            Events : DebugEvent list
            Cancelled : bool
        }

    let private runSteps
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (cancellationToken : System.Threading.CancellationToken)
        (recordLimit : int)
        (maxSteps : int)
        (commitSession : SessionState -> unit)
        (session : SessionState)
        : RunStepsResult
        =
        let events = Queue<DebugEvent> ()
        let mutable session = session
        let mutable stepsRun = 0
        let mutable keepGoing = true

        while not cancellationToken.IsCancellationRequested
              && keepGoing
              && stepsRun < maxSteps do
            let nextSession, event, countedStep = stepSession loggerFactory logger session
            session <- nextSession
            commitSession session

            if events.Count = recordLimit then
                events.Dequeue () |> ignore<DebugEvent>

            events.Enqueue event

            if countedStep then
                stepsRun <- stepsRun + 1

            match session with
            | SessionState.Running _ when countedStep -> ()
            | _ -> keepGoing <- false

        {
            Session = session
            StepsRun = stepsRun
            Events = events |> Seq.toList
            Cancelled = cancellationToken.IsCancellationRequested && keepGoing && stepsRun < maxSteps
        }

    /// How much of each thread's active frame a trace records.
    [<RequireQualifiedAccess>]
    type private TraceValues =
        /// The stack's shape only. The values stay available through `/thread/{id}`, but only for
        /// the step the session is at.
        | Omitted
        /// The active frame's evaluation stack, arguments and locals, as `/thread/{id}` renders them.
        | ActiveFrame

    type private TraceRequest =
        {
            MaxSteps : int
            MaxBytes : int
            Values : TraceValues
        }

    let private traceDefaultMaxSteps : int = 10000
    let private traceStepCap : int = 100000
    let private traceByteCap : int = 32 * 1024 * 1024

    /// A request above a cap is clamped to it, as `/run` does; one that is not a positive integer,
    /// or names an unknown values mode, is refused rather than silently defaulted.
    let private parseTraceRequest (query : IQueryCollection) : Result<TraceRequest, string> =
        let positive (name : string) (defaultValue : int) (cap : int) : Result<int, string> =
            match query.TryGetValue name with
            | false, _ -> Ok defaultValue
            | true, raw ->
                match Int64.TryParse (raw.ToString ()) with
                | true, value when value > 0L -> Ok (int (min value (int64 cap)))
                | _ -> Error $"%s{name} must be a positive integer, not '%s{raw.ToString ()}'"

        let values =
            match query.TryGetValue "values" with
            | false, _ -> Ok TraceValues.Omitted
            | true, raw ->
                match raw.ToString () with
                | "none" -> Ok TraceValues.Omitted
                | "active" -> Ok TraceValues.ActiveFrame
                | other -> Error $"values must be 'none' or 'active', not '%s{other}'"

        match
            positive "maxSteps" traceDefaultMaxSteps traceStepCap, positive "maxBytes" traceByteCap traceByteCap, values
        with
        | Ok maxSteps, Ok maxBytes, Ok values ->
            Ok
                {
                    MaxSteps = maxSteps
                    MaxBytes = maxBytes
                    Values = values
                }
        | Error e, _, _
        | _, Error e, _
        | _, _, Error e -> Error e

    /// Numbers distinct strings in order of first appearance, so that a trace page names each once
    /// in a table and thereafter by its index.
    type private Interner () =
        let indices = Dictionary<string, int> (StringComparer.Ordinal)
        let items = ResizeArray<string> ()
        let mutable characters = 0L

        member _.Intern (item : string) : int =
            match indices.TryGetValue item with
            | true, index -> index
            | false, _ ->
                let index = items.Count
                indices.Add (item, index)
                items.Add item
                characters <- characters + int64 item.Length
                index

        member _.Items : IReadOnlyList<string> = items :> IReadOnlyList<string>

        /// The total length of the distinct strings, which approximates the table's size as JSON.
        member _.Characters : int64 = characters

    /// Renders one JSON value at a time to compact text, which is how the tables of rendered values
    /// are keyed.
    type private JsonRenderer () =
        let buffer = ArrayBufferWriter<byte> ()
        let writer = new Utf8JsonWriter (buffer :> IBufferWriter<byte>)

        member _.Render (write : Utf8JsonWriter -> unit) : string =
            buffer.Clear ()
            writer.Reset (buffer :> IBufferWriter<byte>)
            write writer
            writer.Flush ()
            Encoding.UTF8.GetString buffer.WrittenSpan

        interface IDisposable with
            member _.Dispose () = writer.Dispose ()

    type private TraceTables =
        {
            /// Method and assembly names.
            Strings : Interner
            /// `writeThreadStatus`'s renderings.
            Statuses : Interner
            /// `writeSourceLocationValue`'s renderings.
            Locations : Interner
            /// Step events without their step number or output.
            Events : Interner
            /// `writeEvalStackValue`'s and `writeCliType`'s renderings.
            Values : Interner
        }

        static member Empty () : TraceTables =
            {
                Strings = Interner ()
                Statuses = Interner ()
                Locations = Interner ()
                Events = Interner ()
                Values = Interner ()
            }

        member this.Characters : int64 =
            this.Strings.Characters
            + this.Statuses.Characters
            + this.Locations.Characters
            + this.Events.Characters
            + this.Values.Characters

    /// One frame as a trace reports it, with its strings replaced by table indices.
    type private TracedFrame =
        {
            Id : int
            /// The state this was projected from. The state is immutable, so a frame whose state
            /// and attribution offset are the same objects as at the last step projects the same.
            Frame : MethodState
            AttributionOffset : int
            Method : int
            IlOffset : int
            Location : int option
        }

    type private TracedValues =
        {
            EvalStack : int array
            Arguments : int array
            Locals : int array
            /// The collections these were projected from. They are immutable, and besides the value a
            /// rendering reads only facts that never change once they hold (a heap object's type,
            /// since nothing is freed, and a concrete type's description, since types are only
            /// added), so an unchanged collection projects the same.
            EvalStackSource : EvalStack
            ArgumentsSource : CliType ImmutableArray
            LocalsSource : CliType ImmutableArray
        }

    /// One thread as a trace reports it, with its strings replaced by table indices.
    type private TracedThread =
        {
            /// The state this was projected from, which as for `TracedFrame.Frame` lets an
            /// unchanged thread skip projection.
            Thread : ThreadState
            /// The kernel's record for the thread, which a `BlockedInSyscall` status is rendered
            /// from and which can change while the thread's own state does not.
            Task : UnixTaskState option
            Status : int
            Assembly : int option
            ActiveFrame : int option
            Frames : TracedFrame array
            /// `None` exactly when values were not asked for or there is no active frame.
            Values : TracedValues option
        }

    let private sameTask (a : UnixTaskState option) (b : UnixTaskState option) : bool =
        match a, b with
        | None, None -> true
        | Some a, Some b -> Object.ReferenceEquals (a, b)
        | Some _, None
        | None, Some _ -> false

    let private projectFrame
        (renderer : JsonRenderer)
        (tables : TraceTables)
        (state : IlMachineState)
        (frameId : FrameId)
        (attributionOffset : int)
        (frame : MethodState)
        : TracedFrame
        =
        {
            Id = frameIdValue frameId
            Frame = frame
            AttributionOffset = attributionOffset
            Method = tables.Strings.Intern (string frame.ExecutingMethod)
            IlOffset = frame.IlOpIndex
            Location =
                GuestLocation.trySourceOf state frame.ExecutingMethod attributionOffset
                |> Option.map (fun location ->
                    renderer.Render (fun writer -> writeSourceLocationValue writer attributionOffset location)
                    |> tables.Locations.Intern
                )
        }

    /// `previous` is the thread's values at the step before, whose parts are reused wherever the
    /// collection they were projected from is unchanged.
    let private projectValues
        (renderer : JsonRenderer)
        (tables : TraceTables)
        (context : DebuggerValueContext)
        (previous : TracedValues option)
        (frame : MethodState)
        : TracedValues
        =
        let evalStackValue (value : EvalStackValue) : int =
            renderer.Render (fun writer -> writeEvalStackValue writer context value)
            |> tables.Values.Intern

        let cliValues (previous : (CliType ImmutableArray * int array) option) (values : CliType ImmutableArray) =
            match previous with
            // `ImmutableArray.Equals` compares the underlying arrays by reference.
            | Some (source, indices) when source.Equals values -> indices
            | Some _
            | None ->
                values
                |> Seq.map (fun value ->
                    renderer.Render (fun writer -> writeCliType writer context value)
                    |> tables.Values.Intern
                )
                |> Seq.toArray

        {
            EvalStack =
                match previous with
                | Some previous when Object.ReferenceEquals (previous.EvalStackSource, frame.EvaluationStack) ->
                    previous.EvalStack
                | Some _
                | None -> frame.EvaluationStack.Values |> Seq.map evalStackValue |> Seq.toArray
            Arguments =
                cliValues
                    (previous
                     |> Option.map (fun previous -> previous.ArgumentsSource, previous.Arguments))
                    frame.Arguments
            Locals =
                cliValues
                    (previous |> Option.map (fun previous -> previous.LocalsSource, previous.Locals))
                    frame.LocalVariables
            EvalStackSource = frame.EvaluationStack
            ArgumentsSource = frame.Arguments
            LocalsSource = frame.LocalVariables
        }

    /// `previous` is this thread's projection at the step before, whose parts are reused wherever
    /// the state they were projected from is unchanged.
    let private projectThread
        (renderer : JsonRenderer)
        (tables : TraceTables)
        (values : TraceValues)
        (state : IlMachineState)
        (previous : TracedThread option)
        (threadId : ThreadId)
        (thread : ThreadState)
        : TracedThread
        =
        let task = state.Kernel.Tasks |> Map.tryFind threadId

        let unchangedThread =
            match previous with
            | Some previous -> Object.ReferenceEquals (previous.Thread, thread)
            | None -> false

        match previous with
        | Some previous when unchangedThread && sameTask previous.Task task -> previous
        | _ ->

        let status =
            renderer.Render (fun writer -> writeThreadStatus writer task thread.Status)
            |> tables.Statuses.Intern

        match previous with
        | Some previous when unchangedThread ->
            { previous with
                Task = task
                Status = status
            }
        | _ ->

        let reusable = Dictionary<int, TracedFrame> ()

        match previous with
        | Some previous ->
            for frame in previous.Frames do
                reusable.[frame.Id] <- frame
        | None -> ()

        let offsets = GuestLocation.attributionOffsets thread

        let frames =
            thread.MethodStates
            |> Seq.map (fun (KeyValue (frameId, frame)) ->
                let offset = Map.find frameId offsets

                match reusable.TryGetValue (frameIdValue frameId) with
                | true, traced when
                    Object.ReferenceEquals (traced.Frame, frame)
                    && traced.AttributionOffset = offset
                    ->
                    traced
                | _ -> projectFrame renderer tables state frameId offset frame
            )
            |> Seq.toArray

        let hasActiveFrame = not (ThreadStatus.hasNoActiveFrame thread.Status)

        {
            Thread = thread
            Task = task
            Status = status
            Assembly =
                if hasActiveFrame then
                    Some (tables.Strings.Intern thread.ActiveAssemblyFullName)
                else
                    None
            ActiveFrame =
                if hasActiveFrame then
                    Some (frameIdValue thread.ActiveMethodState)
                else
                    None
            Frames = frames
            Values =
                match values with
                | TraceValues.ActiveFrame when hasActiveFrame ->
                    let previousValues = previous |> Option.bind (fun previous -> previous.Values)

                    Some (
                        projectValues
                            renderer
                            tables
                            (DebuggerValueJson.ofState state)
                            previousValues
                            thread.MethodState
                    )
                | TraceValues.ActiveFrame
                | TraceValues.Omitted -> None
        }

    [<RequireQualifiedAccess>]
    type private Change<'a> =
        | Unchanged
        | ChangedTo of 'a

    let private change<'a when 'a : equality> (previous : 'a option) (next : 'a) : Change<'a> =
        match previous with
        | Some previous when previous = next -> Change.Unchanged
        | Some _
        | None -> Change.ChangedTo next

    /// What changed about one thread between two steps (or, from no previous projection, what it
    /// is). See `writeTracePageProperties` for how a client applies it.
    type private ThreadDelta =
        {
            Thread : int
            Status : Change<int>
            Assembly : Change<int option>
            ActiveFrame : Change<int option>
            Pop : int
            Set : TracedFrame list
            Push : TracedFrame list
            EvalStack : Change<int array>
            Arguments : Change<int array>
            Locals : Change<int array>
        }

        member this.IsEmpty : bool =
            this.Status = Change.Unchanged
            && this.Assembly = Change.Unchanged
            && this.ActiveFrame = Change.Unchanged
            && this.Pop = 0
            && this.Set.IsEmpty
            && this.Push.IsEmpty
            && this.EvalStack = Change.Unchanged
            && this.Arguments = Change.Unchanged
            && this.Locals = Change.Unchanged

    let private threadDelta (threadId : ThreadId) (previous : TracedThread option) (next : TracedThread) : ThreadDelta =
        let previousFrames =
            match previous with
            | Some previous -> previous.Frames
            | None -> Array.empty

        // Frames are identified by id and method together. Ids are never reused within a thread, so
        // the method is only a guard: a retained frame is then updated in place, never replaced.
        let retained =
            Seq.zip previousFrames next.Frames
            |> Seq.takeWhile (fun (before, after) -> before.Id = after.Id && before.Method = after.Method)
            |> Seq.length

        let activeFrame =
            change (previous |> Option.map (fun previous -> previous.ActiveFrame)) next.ActiveFrame

        // A new active frame's values are stated in full, so a client need not remember any frame's
        // values but the active one's.
        let valuesBefore =
            match activeFrame, previous with
            | Change.Unchanged, Some previous -> previous.Values
            | Change.Unchanged, None
            | Change.ChangedTo _, _ -> None

        let valuesPart (select : TracedValues -> int array) : Change<int array> =
            match next.Values with
            | None -> Change.Unchanged
            | Some values -> change (valuesBefore |> Option.map select) (select values)

        {
            Thread = threadIdValue threadId
            Status = change (previous |> Option.map (fun previous -> previous.Status)) next.Status
            Assembly = change (previous |> Option.map (fun previous -> previous.Assembly)) next.Assembly
            ActiveFrame = activeFrame
            Pop = previousFrames.Length - retained
            Set =
                [
                    for i in 0 .. retained - 1 do
                        let before = previousFrames.[i]
                        let after = next.Frames.[i]

                        if before.IlOffset <> after.IlOffset || before.Location <> after.Location then
                            yield after
                ]
            Push = next.Frames |> Array.skip retained |> Array.toList
            EvalStack = valuesPart (fun values -> values.EvalStack)
            Arguments = valuesPart (fun values -> values.Arguments)
            Locals = valuesPart (fun values -> values.Locals)
        }

    let private writeOptionalNumberValue (writer : Utf8JsonWriter) (value : int option) : unit =
        match value with
        | Some value -> writer.WriteNumberValue value
        | None -> writer.WriteNullValue ()

    let private writeChange<'a>
        (writer : Utf8JsonWriter)
        (name : string)
        (write : Utf8JsonWriter -> 'a -> unit)
        (value : Change<'a>)
        : unit
        =
        match value with
        | Change.Unchanged -> ()
        | Change.ChangedTo value ->
            writer.WritePropertyName name
            write writer value

    let private writeIndices (writer : Utf8JsonWriter) (indices : int array) : unit =
        writer.WriteStartArray ()

        for index in indices do
            writer.WriteNumberValue index

        writer.WriteEndArray ()

    let private writeThreadDelta (writer : Utf8JsonWriter) (delta : ThreadDelta) : unit =
        writer.WriteStartObject ()
        writer.WriteNumber ("id", delta.Thread)
        writeChange writer "s" (fun writer (status : int) -> writer.WriteNumberValue status) delta.Status
        writeChange writer "a" writeOptionalNumberValue delta.Assembly
        writeChange writer "f" writeOptionalNumberValue delta.ActiveFrame

        if delta.Pop > 0 then
            writer.WriteNumber ("pop", delta.Pop)

        if not delta.Set.IsEmpty then
            writer.WriteStartArray "set"

            for frame in delta.Set do
                writer.WriteStartArray ()
                writer.WriteNumberValue frame.Id
                writer.WriteNumberValue frame.IlOffset
                writeOptionalNumberValue writer frame.Location
                writer.WriteEndArray ()

            writer.WriteEndArray ()

        if not delta.Push.IsEmpty then
            writer.WriteStartArray "push"

            for frame in delta.Push do
                writer.WriteStartArray ()
                writer.WriteNumberValue frame.Id
                writer.WriteNumberValue frame.Method
                writer.WriteNumberValue frame.IlOffset
                writeOptionalNumberValue writer frame.Location
                writer.WriteEndArray ()

            writer.WriteEndArray ()

        match delta.EvalStack, delta.Arguments, delta.Locals with
        | Change.Unchanged, Change.Unchanged, Change.Unchanged -> ()
        | _ ->
            writer.WriteStartObject "v"
            writeChange writer "e" writeIndices delta.EvalStack
            writeChange writer "a" writeIndices delta.Arguments
            writeChange writer "l" writeIndices delta.Locals
            writer.WriteEndObject ()

        writer.WriteEndObject ()

    /// Writes the `th` and `gone` properties taking a client from `previous` to `next`, or nothing
    /// if no thread changed.
    let private writeThreadDeltas
        (writer : Utf8JsonWriter)
        (previous : Map<ThreadId, TracedThread>)
        (next : Map<ThreadId, TracedThread>)
        : unit
        =
        let deltas =
            next
            |> Map.toSeq
            |> Seq.map (fun (threadId, thread) -> threadDelta threadId (Map.tryFind threadId previous) thread)
            |> Seq.filter (fun delta -> not delta.IsEmpty)
            |> Seq.toList

        if not deltas.IsEmpty then
            writeValueArray writer "th" deltas writeThreadDelta

        let gone =
            previous
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.filter (fun threadId -> not (Map.containsKey threadId next))
            |> Seq.toList

        if not gone.IsEmpty then
            writeValueArray writer "gone" gone (fun writer threadId -> writer.WriteNumberValue (threadIdValue threadId))

    type private HeapCounts = int * int * int

    let private heapCounts (heap : ManagedHeap) : HeapCounts =
        HeapObserver.nonArrayObjectCount heap, HeapObserver.arrayCount heap, HeapObserver.stringContentCount heap

    let private writeHeapCounts (writer : Utf8JsonWriter) (nonArrayObjects : int, arrays : int, strings : int) : unit =
        writer.WriteStartArray "hp"
        writer.WriteNumberValue nonArrayObjects
        writer.WriteNumberValue arrays
        writer.WriteNumberValue strings
        writer.WriteEndArray ()

    let private stepsExecuted (session : SessionState) : int64 =
        match session with
        | SessionState.Running (_, steps)
        | SessionState.Finished (_, steps)
        | SessionState.Deadlocked (_, _, steps) -> steps

    /// Why a trace page ended.
    [<RequireQualifiedAccess>]
    type private TraceStop =
        | StepLimit
        /// The page reached its byte budget. A page takes at least one step whatever its budget, so
        /// that paging always makes progress.
        | ByteBudget
        | Cancelled
        /// The session is finished or deadlocked, so no further page will take a step.
        | SessionEnded
        /// A step threw. The session is left before that step, as `/run` leaves it.
        | HostFailure of exn

    type private TracePage =
        {
            Request : TraceRequest
            FirstStep : int64
            StepsRun : int
            Stop : TraceStop
            Session : SessionState
            /// The JSON of the base record.
            Base : byte[]
            /// The JSON of the array of step records.
            Steps : byte[]
            /// The JSON of the record of what discovering a deadlock changed, if this page did.
            Deadlock : byte[] option
            Tables : TraceTables
        }

    /// Where a trace has got to: the session and the projection its next step's deltas are
    /// relative to.
    type private TraceCursor =
        {
            Session : SessionState
            StepsRun : int
            Threads : Map<ThreadId, TracedThread>
            Heap : ManagedHeap
            HeapCounts : HeapCounts
        }

    /// Advances the session by up to `request.MaxSteps` steps, recording each as a delta, and
    /// committing the session after each as `runSteps` does.
    let private recordTrace
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (cancellationToken : System.Threading.CancellationToken)
        (request : TraceRequest)
        (commitSession : SessionState -> unit)
        (session : SessionState)
        : TracePage
        =
        use renderer = new JsonRenderer ()
        let tables = TraceTables.Empty ()
        let baseBuffer = ArrayBufferWriter<byte> ()
        let stepsBuffer = ArrayBufferWriter<byte> ()
        use baseWriter = new Utf8JsonWriter (baseBuffer :> IBufferWriter<byte>)
        use stepsWriter = new Utf8JsonWriter (stepsBuffer :> IBufferWriter<byte>)

        let project (previous : Map<ThreadId, TracedThread>) (state : IlMachineState) : Map<ThreadId, TracedThread> =
            state.ThreadState
            |> Map.map (fun threadId thread ->
                projectThread renderer tables request.Values state (Map.tryFind threadId previous) threadId thread
            )

        let initialState = sessionState session

        let start =
            {
                Session = session
                StepsRun = 0
                Threads = project Map.empty initialState
                Heap = initialState.ManagedHeap
                HeapCounts = heapCounts initialState.ManagedHeap
            }

        baseWriter.WriteStartObject ()
        writeHeapCounts baseWriter start.HeapCounts
        writeThreadDeltas baseWriter Map.empty start.Threads
        baseWriter.WriteEndObject ()
        baseWriter.Flush ()

        stepsWriter.WriteStartArray ()

        let pageBytes () : int64 =
            int64 baseBuffer.WrittenCount
            + stepsWriter.BytesCommitted
            + int64 stepsWriter.BytesPending
            + tables.Characters

        /// Writes the record taking a client from `cursor` to `session`, which `event` produced.
        let writeRecord
            (writer : Utf8JsonWriter)
            (cursor : TraceCursor)
            (session : SessionState)
            (event : DebugEvent)
            : TraceCursor
            =
            let state = sessionState session
            writer.WriteStartObject ()

            let eventIndex =
                renderer.Render (fun writer ->
                    writer.WriteStartObject ()
                    writeEventProperties writer event
                    writer.WriteEndObject ()
                )
                |> tables.Events.Intern

            writer.WriteNumber ("e", eventIndex)

            match event.Effect with
            | StepEffect.NoEffect -> ()
            | StepEffect.WroteToFd (role, bytes) ->
                writer.WritePropertyName "o"
                writeOutputEntry writer role bytes

            let counts =
                if Object.ReferenceEquals (cursor.Heap, state.ManagedHeap) then
                    cursor.HeapCounts
                else
                    heapCounts state.ManagedHeap

            if counts <> cursor.HeapCounts then
                writeHeapCounts writer counts

            let threads = project cursor.Threads state
            writeThreadDeltas writer cursor.Threads threads
            writer.WriteEndObject ()

            {
                Session = session
                StepsRun = cursor.StepsRun
                Threads = threads
                Heap = state.ManagedHeap
                HeapCounts = counts
            }

        let deadlockBuffer = ArrayBufferWriter<byte> ()
        use deadlockWriter = new Utf8JsonWriter (deadlockBuffer :> IBufferWriter<byte>)

        let rec advance (cursor : TraceCursor) : TraceCursor * TraceStop =
            match cursor.Session with
            | SessionState.Finished _
            | SessionState.Deadlocked _ -> cursor, TraceStop.SessionEnded
            | SessionState.Running _ ->

            if cursor.StepsRun >= request.MaxSteps then
                cursor, TraceStop.StepLimit
            elif cursor.StepsRun > 0 && pageBytes () >= int64 request.MaxBytes then
                cursor, TraceStop.ByteBudget
            elif cancellationToken.IsCancellationRequested then
                cursor, TraceStop.Cancelled
            else

            let stepped =
                try
                    Ok (stepSession loggerFactory logger cursor.Session)
                with ex ->
                    Error ex

            match stepped with
            | Error ex -> cursor, TraceStop.HostFailure ex
            | Ok (session, event, countedStep) ->
                commitSession session

                if countedStep then
                    let cursor = writeRecord stepsWriter cursor session event

                    advance
                        { cursor with
                            StepsRun = cursor.StepsRun + 1
                        }
                else
                    // Only the discovery of a deadlock is not a step. It executed no instruction,
                    // but looking for one can still change state — a timed wait that expires moves
                    // its thread on to reacquiring the lock — so it gets a record of its own.
                    advance (writeRecord deadlockWriter cursor session event)

        let finish, stop = advance start

        stepsWriter.WriteEndArray ()
        stepsWriter.Flush ()
        deadlockWriter.Flush ()

        {
            Request = request
            FirstStep = stepsExecuted session
            StepsRun = finish.StepsRun
            Stop = stop
            Session = finish.Session
            Base = baseBuffer.WrittenSpan.ToArray ()
            Steps = stepsBuffer.WrittenSpan.ToArray ()
            Deadlock =
                if deadlockBuffer.WrittenCount = 0 then
                    None
                else
                    Some (deadlockBuffer.WrittenSpan.ToArray ())
            Tables = tables
        }

    /// A trace page's properties, for the caller to wrap in an object, beside a failure's if the
    /// page ended in one.
    ///
    /// A page is self-contained: `base` is the state before its first step, stated as deltas from
    /// nothing, and every index refers to this page's own tables. Replaying pages one after another
    /// therefore gives the same states as one page covering the same steps, whatever the page
    /// boundaries; each page's `base` equals the state its predecessor ended in.
    ///
    /// `steps[i]` is step `firstStep + i + 1`, and records the state after it:
    /// - `e`: index into `events`, what the step did (`kind`, `thread`, `detail`, as `/step` reports);
    /// - `o`: what the step wrote, as `/step`'s `output`; absent if nothing;
    /// - `hp`: `[nonArrayObjects, arrays, stringContents]`, present in `base` and when it changed;
    /// - `th`: a delta for each thread that changed, which a client applies to its copy of that thread;
    /// - `gone`: ids of threads that no longer exist.
    ///
    /// `deadlock` is null unless this page discovered that the guest is deadlocked, in which case it
    /// is a record of the same shape, applied after the last step. It is not a step and does not
    /// count towards `stepsRun`: it executed no instruction, but the scheduler looking for one can
    /// still change state, as when a timed wait expires into reacquiring a lock that is held.
    ///
    /// A thread delta has `id`, and then only what changed, in this order:
    /// - `s`: index into `statuses`; `a`: index into `strings` of the active assembly, or null;
    ///   `f`: the active frame's id, or null. A thread's first delta carries all three.
    /// - `pop`: how many frames to remove from the top (the end of the list);
    /// - `set`: `[id, ilOffset, location]` for retained frames whose offset or location changed;
    /// - `push`: `[id, method, ilOffset, location]` frames to append, `method` indexing `strings`.
    ///   A `location` is an index into `locations`, or null. Frames are listed outermost first, as
    ///   `/thread/{id}` lists them.
    /// - `v`, with `values=active` only: the active frame's `e`valuation stack, `a`rguments and
    ///   `l`ocals, each an array of indices into `frameValues`, and each present only if it changed —
    ///   except that all three are present whenever `f` is.
    let private writeTracePageProperties (writer : Utf8JsonWriter) (page : TracePage) : unit =
        writer.WriteNumber ("maxSteps", page.Request.MaxSteps)
        writer.WriteNumber ("maxBytes", page.Request.MaxBytes)

        writer.WriteString (
            "values",
            match page.Request.Values with
            | TraceValues.Omitted -> "none"
            | TraceValues.ActiveFrame -> "active"
        )

        writer.WriteNumber ("firstStep", page.FirstStep)
        writer.WriteNumber ("stepsRun", page.StepsRun)

        writer.WriteString (
            "stoppedBecause",
            match page.Stop with
            | TraceStop.StepLimit -> "stepLimit"
            | TraceStop.ByteBudget -> "byteBudget"
            | TraceStop.Cancelled -> "cancelled"
            | TraceStop.SessionEnded -> "sessionEnded"
            | TraceStop.HostFailure _ -> "hostFailure"
        )

        writer.WritePropertyName "base"
        writer.WriteRawValue (ReadOnlySpan<byte> page.Base, true)
        writer.WritePropertyName "steps"
        writer.WriteRawValue (ReadOnlySpan<byte> page.Steps, true)
        writer.WritePropertyName "deadlock"

        match page.Deadlock with
        | Some deadlock -> writer.WriteRawValue (ReadOnlySpan<byte> deadlock, true)
        | None -> writer.WriteNullValue ()

        writeValueArray writer "strings" page.Tables.Strings.Items (fun writer s -> writer.WriteStringValue s)

        let writeRenderedTable (name : string) (table : Interner) : unit =
            writeValueArray writer name table.Items (fun writer (json : string) -> writer.WriteRawValue (json, true))

        writeRenderedTable "statuses" page.Tables.Statuses
        writeRenderedTable "locations" page.Tables.Locations
        writeRenderedTable "events" page.Tables.Events
        writeRenderedTable "frameValues" page.Tables.Values
        writeSessionSummary writer page.Session

    let private helpText (baseUrl : string) : string =
        String.concat
            Environment.NewLine
            [
                $"PawPrint debugger server at %s{baseUrl}"
                "GET  /state"
                "POST /step?count=1"
                "POST /run?maxSteps=10000"
                $"POST /trace?maxSteps=%d{traceDefaultMaxSteps}&maxBytes=%d{traceByteCap}&values=none|active"
                $"     (at most %d{traceStepCap} steps and about %d{traceByteCap} bytes a page; repeat to page on)"
                "GET  /thread/{id}"
                "GET  /thread/{id}/stack-summary"
                "GET  /thread/{id}/active-method/il"
                "GET  /heap?after=<address>&limit=200"
                "GET  /heap/{address}"
                "GET  /statics"
                "GET  /output"
                "POST /reset"
                "POST /stop"
            ]

    let private requestBaseUrl (context : HttpContext) : string =
        let pathBase = context.Request.PathBase.ToString ()
        $"{context.Request.Scheme}://{context.Request.Host}%s{pathBase}/"

    let internal configureLoopbackEphemeralPort (webHost : IWebHostBuilder) : unit =
        webHost.ConfigureKestrel (fun options -> options.Listen (IPAddress.Loopback, 0))
        |> ignore<IWebHostBuilder>

    let internal baseUrl (app : WebApplication) : string =
        let server = app.Services.GetRequiredService<IServer> ()
        let addresses = server.Features.Get<IServerAddressesFeature> ()

        if isNull addresses || addresses.Addresses.Count <> 1 then
            failwith $"Expected exactly one debugger server address, got %O{addresses}"

        let address = addresses.Addresses |> Seq.exactlyOne

        if address.EndsWith ("/", StringComparison.Ordinal) then
            address
        else
            address + "/"

    let internal createApp
        (loggerFactory : ILoggerFactory)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (kernelConfig : KernelConfig)
        (pctSeed : uint64 option)
        (argv : string list)
        (token : string)
        (configureWebHost : IWebHostBuilder -> unit)
        : WebApplication * System.Threading.CancellationTokenSource
        =
        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App.DebuggerServer"

        let mutable session =
            prepareSession loggerFactory dllPath dotnetRuntimeDirs kernelConfig pctSeed argv

        let sessionLock = obj ()
        let stopStateLock = obj ()
        let mutable stopRequested = false
        let mutable activeStepRequests = 0

        let builder = WebApplication.CreateBuilder [||]
        builder.Logging.ClearProviders () |> ignore<ILoggingBuilder>

        configureWebHost builder.WebHost

        let app = builder.Build ()
        let stopCts = new System.Threading.CancellationTokenSource ()

        let requestStop () : unit =
            lock
                stopStateLock
                (fun () ->
                    stopRequested <- true
                    stopCts.Cancel ()
                )

        let isStopRequested () : bool =
            lock stopStateLock (fun () -> stopRequested)

        app.Use (fun (context : HttpContext) (next : RequestDelegate) ->
            task {
                if isAuthorised token context then
                    return! next.Invoke context
                else
                    do! writeResponse context unauthorisedResponse
            }
            :> Task
        )
        |> ignore<IApplicationBuilder>

        let responseOnly (response : DebuggerHttpResponse) : HandlerResult =
            {
                Response = response
                StopAfterResponse = false
                ReleaseActiveStepRequestAfterResponse = false
            }

        let stoppingResponse (response : DebuggerHttpResponse) : HandlerResult =
            {
                Response = response
                StopAfterResponse = true
                ReleaseActiveStepRequestAfterResponse = false
            }

        let stepResponse (response : DebuggerHttpResponse) : HandlerResult =
            {
                Response = response
                StopAfterResponse = false
                ReleaseActiveStepRequestAfterResponse = true
            }

        app.Run (fun context ->
            task {
                let method = context.Request.Method.ToUpperInvariant ()

                let segments =
                    match context.Request.Path.Value with
                    | null -> []
                    | path -> pathSegments path

                let isStopRequest =
                    match method, segments with
                    | "POST", [ "stop" ] -> true
                    | _ -> false

                let result =
                    if isStopRequest then
                        requestStop ()

                        if System.Threading.Volatile.Read (&activeStepRequests) = 0 then
                            stoppingResponse (textResponse 200 "stopping")
                        else
                            responseOnly (textResponse 200 "stopping")
                    else
                        try
                            lock
                                sessionLock
                                (fun () ->
                                    match method, segments with
                                    | "GET", []
                                    | "GET", [ "help" ] ->
                                        responseOnly (textResponse 200 (helpText (requestBaseUrl context)))
                                    | "GET", [ "state" ] ->
                                        responseOnly (
                                            jsonResponse 200 (fun writer -> writeStateResponse writer session)
                                        )
                                    | "GET", [ "output" ] ->
                                        responseOnly (
                                            jsonResponse 200 (fun writer -> writeOutputResponse writer session)
                                        )
                                    | "POST", [ "step" ] ->
                                        let count = parsePositiveInt "count" 1 1000 context.Request.Query

                                        System.Threading.Interlocked.Increment (&activeStepRequests) |> ignore<int>

                                        let mutable releaseAfterResponse = false

                                        try
                                            try
                                                let result =
                                                    runSteps
                                                        loggerFactory
                                                        logger
                                                        stopCts.Token
                                                        count
                                                        count
                                                        (fun nextSession -> session <- nextSession)
                                                        session

                                                session <- result.Session

                                                let response =
                                                    jsonResponse
                                                        200
                                                        (fun writer ->
                                                            writer.WriteStartObject ()
                                                            writer.WriteNumber ("requestedSteps", count)
                                                            writer.WriteNumber ("stepsRun", result.StepsRun)
                                                            writer.WriteBoolean ("cancelled", result.Cancelled)
                                                            writeValueArray writer "events" result.Events writeEvent
                                                            writeSessionSummary writer session
                                                            writer.WriteEndObject ()
                                                        )

                                                releaseAfterResponse <- true
                                                stepResponse response
                                            with ex ->
                                                logger.LogError (ex, "Debugger step request failed")
                                                let response = requestFailureResponse "step" ex session
                                                releaseAfterResponse <- true
                                                stepResponse response
                                        finally
                                            if not releaseAfterResponse then
                                                System.Threading.Interlocked.Decrement (&activeStepRequests)
                                                |> ignore<int>
                                    | "POST", [ "run" ] ->
                                        let maxSteps = parsePositiveInt "maxSteps" 10000 1000000 context.Request.Query

                                        System.Threading.Interlocked.Increment (&activeStepRequests) |> ignore<int>

                                        let mutable releaseAfterResponse = false

                                        try
                                            try
                                                let result =
                                                    runSteps
                                                        loggerFactory
                                                        logger
                                                        stopCts.Token
                                                        20
                                                        maxSteps
                                                        (fun nextSession -> session <- nextSession)
                                                        session

                                                session <- result.Session

                                                let response =
                                                    jsonResponse
                                                        200
                                                        (fun writer ->
                                                            writer.WriteStartObject ()
                                                            writer.WriteNumber ("maxSteps", maxSteps)
                                                            writer.WriteNumber ("stepsRun", result.StepsRun)
                                                            writer.WriteBoolean ("cancelled", result.Cancelled)

                                                            writeValueArray
                                                                writer
                                                                "recentEvents"
                                                                result.Events
                                                                writeEvent

                                                            writeSessionSummary writer session
                                                            writer.WriteEndObject ()
                                                        )

                                                releaseAfterResponse <- true
                                                stepResponse response
                                            with ex ->
                                                logger.LogError (ex, "Debugger run request failed")
                                                let response = requestFailureResponse "run" ex session
                                                releaseAfterResponse <- true
                                                stepResponse response
                                        finally
                                            if not releaseAfterResponse then
                                                System.Threading.Interlocked.Decrement (&activeStepRequests)
                                                |> ignore<int>
                                    | "POST", [ "trace" ] ->
                                        match parseTraceRequest context.Request.Query with
                                        | Error message -> responseOnly (textResponse 400 message)
                                        | Ok request ->
                                            System.Threading.Interlocked.Increment (&activeStepRequests)
                                            |> ignore<int>

                                            let mutable releaseAfterResponse = false

                                            try
                                                try
                                                    let page =
                                                        recordTrace
                                                            loggerFactory
                                                            logger
                                                            stopCts.Token
                                                            request
                                                            (fun nextSession -> session <- nextSession)
                                                            session

                                                    session <- page.Session

                                                    let compact = JsonWriterOptions ()

                                                    let response =
                                                        match page.Stop with
                                                        | TraceStop.HostFailure ex ->
                                                            logger.LogError (ex, "Debugger trace request failed")

                                                            jsonResponseWith
                                                                compact
                                                                500
                                                                (fun writer ->
                                                                    writer.WriteStartObject ()
                                                                    writeFailureProperties writer "trace" ex
                                                                    writeTracePageProperties writer page
                                                                    writer.WriteEndObject ()
                                                                )
                                                        | TraceStop.StepLimit
                                                        | TraceStop.ByteBudget
                                                        | TraceStop.Cancelled
                                                        | TraceStop.SessionEnded ->
                                                            jsonResponseWith
                                                                compact
                                                                200
                                                                (fun writer ->
                                                                    writer.WriteStartObject ()
                                                                    writeTracePageProperties writer page
                                                                    writer.WriteEndObject ()
                                                                )

                                                    releaseAfterResponse <- true
                                                    stepResponse response
                                                with ex ->
                                                    logger.LogError (ex, "Debugger trace request failed")
                                                    let response = requestFailureResponse "trace" ex session
                                                    releaseAfterResponse <- true
                                                    stepResponse response
                                            finally
                                                if not releaseAfterResponse then
                                                    System.Threading.Interlocked.Decrement (&activeStepRequests)
                                                    |> ignore<int>
                                    | "GET", [ "thread" ; rawThread ; "stack-summary" ] ->
                                        match Int32.TryParse rawThread with
                                        | true, thread ->
                                            let threadId = ThreadId.ThreadId thread
                                            let statusCode = if hasThread session threadId then 200 else 404

                                            let edgeFrames =
                                                parsePositiveInt "edgeFrames" 12 100 context.Request.Query

                                            let topMethods = parsePositiveInt "topMethods" 8 100 context.Request.Query

                                            jsonResponse
                                                statusCode
                                                (fun writer ->
                                                    writeThreadStackSummaryResponse
                                                        writer
                                                        session
                                                        threadId
                                                        edgeFrames
                                                        topMethods
                                                )
                                            |> responseOnly
                                        | _ -> responseOnly (textResponse 400 $"Invalid thread id: %s{rawThread}")
                                    | "GET", [ "thread" ; rawThread ; "active-method" ; "il" ] ->
                                        match Int32.TryParse rawThread with
                                        | true, thread ->
                                            let threadId = ThreadId.ThreadId thread
                                            let statusCode = if hasThread session threadId then 200 else 404

                                            let instructionContext =
                                                parseOptionalPositiveInt "context" 500 context.Request.Query

                                            jsonResponse
                                                statusCode
                                                (fun writer ->
                                                    writeActiveMethodIlResponse
                                                        writer
                                                        session
                                                        threadId
                                                        instructionContext
                                                )
                                            |> responseOnly
                                        | _ -> responseOnly (textResponse 400 $"Invalid thread id: %s{rawThread}")
                                    | "GET", [ "thread" ; rawThread ] ->
                                        match Int32.TryParse rawThread with
                                        | true, thread ->
                                            let threadId = ThreadId.ThreadId thread
                                            let statusCode = if hasThread session threadId then 200 else 404

                                            jsonResponse
                                                statusCode
                                                (fun writer -> writeThreadResponse writer session threadId)
                                            |> responseOnly
                                        | _ -> responseOnly (textResponse 400 $"Invalid thread id: %s{rawThread}")
                                    | "GET", [ "heap" ] ->
                                        let limit = parsePositiveInt "limit" 200 2000 context.Request.Query
                                        let rawAfter = context.Request.Query.["after"].ToString ()

                                        if String.IsNullOrWhiteSpace rawAfter then
                                            jsonResponse
                                                200
                                                (fun writer -> writeHeapListingResponse writer session None limit)
                                            |> responseOnly
                                        else
                                            match Int32.TryParse rawAfter with
                                            | true, after ->
                                                let after = Some (ManagedHeapAddress.ManagedHeapAddress after)

                                                jsonResponse
                                                    200
                                                    (fun writer ->
                                                        writeHeapListingResponse writer session after limit
                                                    )
                                                |> responseOnly
                                            | false, _ ->
                                                responseOnly (textResponse 400 $"Invalid heap address: %s{rawAfter}")
                                    | "GET", [ "statics" ] ->
                                        responseOnly (
                                            jsonResponse 200 (fun writer -> writeStaticsResponse writer session)
                                        )
                                    | "GET", [ "heap" ; rawAddress ] ->
                                        match Int32.TryParse rawAddress with
                                        | true, address ->
                                            let address = ManagedHeapAddress.ManagedHeapAddress address
                                            let statusCode = if hasHeapAddress session address then 200 else 404

                                            jsonResponse
                                                statusCode
                                                (fun writer -> writeHeapObjectResponse writer session address)
                                            |> responseOnly
                                        | _ -> responseOnly (textResponse 400 $"Invalid heap address: %s{rawAddress}")
                                    | "POST", [ "reset" ] ->
                                        session <-
                                            prepareSession
                                                loggerFactory
                                                dllPath
                                                dotnetRuntimeDirs
                                                kernelConfig
                                                pctSeed
                                                argv

                                        jsonResponse
                                            200
                                            (fun writer ->
                                                writer.WriteStartObject ()
                                                writer.WriteString ("status", "reset")
                                                writeSessionSummary writer session
                                                writer.WriteEndObject ()
                                            )
                                        |> responseOnly
                                    | _ -> responseOnly (textResponse 404 (helpText (requestBaseUrl context)))
                                )
                        with ex ->
                            logger.LogError (ex, "Debugger request failed")
                            responseOnly (textResponse 500 ex.Message)

                do! writeResponse context result.Response

                let mutable remainingActiveStepRequests =
                    System.Threading.Volatile.Read (&activeStepRequests)

                if result.ReleaseActiveStepRequestAfterResponse then
                    remainingActiveStepRequests <- System.Threading.Interlocked.Decrement (&activeStepRequests)

                if
                    result.StopAfterResponse
                    || (result.ReleaseActiveStepRequestAfterResponse
                        && remainingActiveStepRequests = 0
                        && isStopRequested ())
                then
                    context.RequestServices.GetRequiredService<IHostApplicationLifetime>().StopApplication ()
            }
            :> Task
        )

        app, stopCts

    let run
        (loggerFactory : ILoggerFactory)
        (dllPath : string)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (kernelConfig : KernelConfig)
        (pctSeed : uint64 option)
        (argv : string list)
        : int
        =
        let token = generateBearerToken ()

        let app, stopCts =
            createApp
                loggerFactory
                dllPath
                dotnetRuntimeDirs
                kernelConfig
                pctSeed
                argv
                token
                configureLoopbackEphemeralPort

        use _stopCts = stopCts

        app.Start ()

        let baseUrl = baseUrl app

        printfn "PawPrint debugger listening on %s" baseUrl
        printfn "PawPrint debugger bearer token: %s" token
        printfn "Try: curl -H 'Authorization: Bearer %s' %sstate" token baseUrl

        app.WaitForShutdown ()
        0
