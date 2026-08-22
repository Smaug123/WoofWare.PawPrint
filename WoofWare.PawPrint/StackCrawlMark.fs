namespace WoofWare.PawPrint

/// <summary>
/// CoreLib's <c>System.Threading.StackCrawlMark</c>: which frame, relative to the one that declared
/// the mark, a QCall doing a stack crawl is being asked about.
/// </summary>
/// <remarks>
/// The values are CoreLib's own (<c>StackCrawlMark.cs</c>); a mark reaches the native boundary as
/// the <c>Int32</c> contents of a local, wrapped in a <c>StackCrawlMarkHandle</c>.
/// </remarks>
[<RequireQualifiedAccess>]
type StackCrawlMark =
    | LookForMe
    | LookForMyCaller
    | LookForMyCallersCaller
    | LookForThread

/// <summary>
/// The frame walk that CoreCLR's <c>SystemDomain::GetCallersModule</c> performs on behalf of a
/// <c>StackCrawlMark</c>-taking QCall.
/// </summary>
/// <remarks>
/// <para>
/// CoreCLR walks the thread's frames newest-first and compares each frame's stack pointer against
/// the address of the mark, because an address is all a native stack walk has to go on. PawPrint's
/// managed pointers are structured instead: the byref inside a <c>StackCrawlMarkHandle</c> names
/// the frame that owns the mark's storage outright, so "which frame declared the mark" is a lookup
/// rather than a comparison, and the question of how many stubs the two runtimes each interpose
/// between the mark and the native boundary — where they genuinely differ — never arises. The
/// caller supplies that <c>FrameId</c>; this module answers the rest.
/// </para>
/// <para>
/// The walk this module performs is therefore only the part of CoreCLR's that begins *outside* the
/// marked frame: skipping reflection infrastructure, and counting off one frame or two.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module StackCrawlMark =

    /// Decode the <c>Int32</c> CoreLib stores in a <c>StackCrawlMark</c> local.
    let ofInt32 (operation : string) (value : int) : StackCrawlMark =
        match value with
        | 0 -> StackCrawlMark.LookForMe
        | 1 -> StackCrawlMark.LookForMyCaller
        | 2 -> StackCrawlMark.LookForMyCallersCaller
        | 3 -> StackCrawlMark.LookForThread
        | other ->
            failwith
                $"%s{operation}: %i{other} is not a System.Threading.StackCrawlMark; the guest handed a QCall a stack-crawl mark whose storage does not hold one of the four declared values"

    /// The CoreLib types whose frames a stack crawl steps over, from CoreCLR's
    /// `reflectionInvocationTypes` (vm/appdomain.cpp). Namespace-qualified because the check below
    /// tests namespace and name separately; grouped by namespace for the same reason.
    let private reflectionInvocationTypes : Set<string * string> =
        Set.ofList
            [
                "System.Reflection", "RuntimeMethodInfo"
                "System.Reflection", "MethodBase"
                "System.Reflection", "MethodInfo"
                "System.Reflection", "RuntimeConstructorInfo"
                "System.Reflection", "ConstructorInfo"
                "System.Reflection", "RuntimeFieldInfo"
                "System.Reflection", "RtFieldInfo"
                "System.Reflection", "FieldInfo"
                "System.Reflection", "RuntimeEventInfo"
                "System.Reflection", "EventInfo"
                "System.Reflection", "RuntimePropertyInfo"
                "System.Reflection", "PropertyInfo"
                "System.Reflection", "Assembly"
                "System.Reflection", "RuntimeAssembly"
                "System.Reflection", "TypeDelegator"
                "System.Reflection", "MethodBaseInvoker"
                "System", "RuntimeType"
                "System", "RuntimeTypeHandle"
                "System", "RuntimeMethodHandle"
                "System", "RuntimeFieldHandle"
                "System", "Type"
                "System", "Activator"
                "System", "Array"
                "System", "Delegate"
                "System", "MulticastDelegate"
                "System.Runtime.CompilerServices", "RuntimeHelpers"
                "System.Runtime.CompilerServices", "InitHelpers"
                "System.Runtime.CompilerServices", "StaticsHelpers"
                "System.Reflection.Emit", "DynamicMethod"
            ]

    /// <summary>
    /// Whether <paramref name="frame" /> is reflection or invocation infrastructure, which a stack
    /// crawl looks straight through on its way to the caller the guest means.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is CoreCLR's <c>SystemDomain::IsReflectionInvocationMethod</c>, transcribed: an
    /// enumerated list of non-generic CoreLib types, plus any dynamic method named
    /// <c>InvokeStub_*</c>. The list is enumerated from upstream rather than from the paths a test
    /// happens to reach, because a type missing from it does not fail loudly — it makes the crawl
    /// answer <c>System.Private.CoreLib</c> where the guest's own assembly was meant.
    /// </para>
    /// <para>
    /// The generic gate matters: CoreCLR tests <c>!pCaller-&gt;HasInstantiation()</c> before
    /// consulting the list, so <c>Array</c> is infrastructure but a hypothetical
    /// <c>Array&lt;T&gt;</c> would not be.
    /// </para>
    /// </remarks>
    let isReflectionInvocation (frame : MethodState) : bool =
        // CoreCLR's own first test, and it gates the dynamic-method arm as much as the nominal one:
        // a guest is free to name a `DynamicMethod` `InvokeStub_Whatever`, and that must not make
        // its frame invisible to a crawl. Upstream says as much where it recognises the prefix.
        //
        // No guest can reach the dynamic-method arm at all yet, in either direction: for a dynamic
        // method's frame to be live during a crawl its body must call the QCall, and a body whose
        // `Call` names a real method is refused when the method is minted ("holds a
        // System.RuntimeMethodHandle rather than a method"). The arm is written from upstream
        // rather than from what a test reaches, for the reason the list below is.
        AssemblyDefinitionName.isNamed "System.Private.CoreLib" frame.ExecutingMethod.DeclaringAssemblyFullName
        && match frame.ExecutingMethod.TryDeclaringType with
           | None ->
               // A dynamic method. CoreCLR treats an LCG method as infrastructure exactly when it is
               // one of the reflection invoke stubs, which it recognises by name prefix; every other
               // dynamic method is guest code and answers with its scope assembly.
               frame.ExecutingMethod.Name.StartsWith ("InvokeStub_", System.StringComparison.Ordinal)
           | Some declaringType ->
               declaringType.Generics.IsEmpty
               && reflectionInvocationTypes.Contains (declaringType.Namespace, declaringType.Name)

    /// <summary>
    /// The frame a stack crawl for <paramref name="mark" /> answers with, where
    /// <paramref name="markFrame" /> is the frame whose storage holds the mark.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Refuses <c>LookForMe</c> and <c>LookForThread</c>. Neither is constructible at any QCall that
    /// reaches this: every <c>StackCrawlMark.LookFor*</c> in CoreLib is <c>LookForMyCaller</c> or
    /// (at <c>Assembly.GetCallingAssembly</c>, alone) <c>LookForMyCallersCaller</c>; the one
    /// <c>default</c>-valued mark, in <c>RuntimeAssembly.InternalGetSatelliteAssembly</c>, is passed
    /// to a QCall that ignores it. An arm no input can reach is an arm no test can pin, so a mark
    /// that becomes reachable should stop here rather than take an unexercised path.
    /// </para>
    /// <para>
    /// Refuses while a first-pass handler search is suspended in a filter, for the reason
    /// <see cref="StackFrameCapture.ofThread" /> gives at more length: the frames between the throw
    /// and the filter's host are live but off the host's return chain, so what CoreCLR's physical
    /// walk would traverse and what this chain walk traverses are different sets of frames.
    /// </para>
    /// </remarks>
    let resolveCaller
        (operation : string)
        (mark : StackCrawlMark)
        (markFrame : FrameId)
        (thread : ThreadState)
        : MethodState
        =
        // How many non-reflection frames outward from the marked frame the answer lies. CoreCLR
        // spells this as a one-round delay before returning; a count says the same thing without
        // the sentinel bookkeeping.
        let wanted =
            match mark with
            | StackCrawlMark.LookForMyCaller -> 1
            | StackCrawlMark.LookForMyCallersCaller -> 2
            | StackCrawlMark.LookForMe
            | StackCrawlMark.LookForThread ->
                failwith
                    $"TODO: %s{operation} was handed a %O{mark} stack-crawl mark, which no CoreLib caller of this QCall constructs. Implementing it means deciding what frame it names under PawPrint's frame chain and finding a guest that can reach it."

        let start =
            ThreadState.tryGetFrame markFrame thread
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: the stack-crawl mark is stored in %O{markFrame}, which is not a live frame of this thread. This is an interpreter bug."
            )

        let refuseIfFiltering (frame : MethodState) : unit =
            frame.ExceptionContinuations
            |> List.iter (fun continuation ->
                match continuation.Continuation with
                | ExceptionContinuation.ResumeAfterFilter _ ->
                    failwith
                        $"%s{operation}: cannot crawl the stack while a first-pass handler search is suspended in a filter of %s{frame.ExecutingMethod.Name}. The frames inner to it are live but are not on its return chain, so this walk and the physical walk CoreCLR performs would cross different frames."
                | ExceptionContinuation.ResumeAfterFinally _
                | ExceptionContinuation.PropagatingException _ -> ()
            )

        // `frame` is where the walk currently stands; `found` and `passed` describe the candidate
        // frames — those that are neither reflection infrastructure nor a delegate stub — seen
        // strictly outside the marked frame. The marked frame is never itself a candidate, matching
        // CoreCLR, which starts testing frames only once its walk has passed the mark.
        //
        // A valid return chain is strictly decreasing in frame id and so cannot cycle; the bound is
        // here so that a chain which *is* malformed says so rather than looping forever.
        let rec walk (remaining : int) (found : MethodState option) (passed : int) (frame : MethodState) : MethodState =
            if remaining < 0 then
                failwith
                    $"%s{operation}: walked more than %d{thread.MethodStates.Count} frames, the number live, without reaching one that has no caller; the return chain from %O{markFrame} does not terminate. This is an interpreter bug."

            refuseIfFiltering frame

            match frame.ReturnState with
            | None ->
                // The stack ran out. CoreCLR's `LookForMyCallersCaller` keeps whatever candidate it
                // last recorded, which is why `Assembly.GetCallingAssembly` is documented as not
                // guaranteed to be right: called directly from a guest's entry point there is no
                // caller's caller, and the answer is the caller. Reproduce that rather than refusing.
                match found with
                | Some found -> found
                | None ->
                    failwith
                        $"%s{operation}: reached the outermost frame of the thread without passing a single frame that is not reflection infrastructure. PawPrint's outermost frame is always guest code, so this is an interpreter bug."
            | Some returnState ->

            match ThreadState.tryGetFrame returnState.JumpTo thread with
            | None ->
                failwith
                    $"%s{operation}: frame %O{returnState.JumpTo} is named as the caller of a live frame but is not itself live. This is an interpreter bug."
            | Some caller ->

            // The delegate-stub check mirrors `StackFrameCapture.ofThread`, for the same reason:
            // real .NET has no frame for a delegate's `Invoke`, so counting one would count a frame
            // that does not exist. Unlike there, no guest reaches it here — PawPrint keeps that
            // synthetic frame alive only across a class initialiser it triggered, and a `.cctor`
            // reached through a delegate is run by the target's own prologue, after the stub frame
            // has been popped. It is kept because the alternative when a guest does reach it is a
            // silently wrong assembly rather than a loud failure.
            if isReflectionInvocation caller || StackFrameCapture.isDelegateInvokeStub caller then
                walk (remaining - 1) found passed caller
            elif passed + 1 >= wanted then
                refuseIfFiltering caller
                caller
            else
                walk (remaining - 1) (Some caller) (passed + 1) caller

        walk thread.MethodStates.Count None 0 start

    /// <summary>
    /// Whether <paramref name="markFrame" /> is <paramref name="frame" /> or one of its callers.
    /// </summary>
    /// <remarks>
    /// The consistency check a <c>StackCrawlMark</c>-taking QCall owes itself. Every mark CoreLib
    /// constructs is a local of a frame the QCall was reached *through*, so a decoded frame that is
    /// not on the return chain means the handle was decoded wrongly — and every reachable guest
    /// puts only skipped-over CoreLib frames between the mark and the native boundary, so a wrong
    /// decode would otherwise produce the right answer for the wrong reason and go unnoticed.
    /// </remarks>
    let isOnReturnChainOf (markFrame : FrameId) (frame : FrameId) (thread : ThreadState) : bool =
        let rec walk (remaining : int) (frame : FrameId) : bool =
            if frame = markFrame then
                true
            elif remaining < 0 then
                false
            else

            match ThreadState.tryGetFrame frame thread with
            | None -> false
            | Some frame ->

            match frame.ReturnState with
            | None -> false
            | Some returnState -> walk (remaining - 1) returnState.JumpTo

        walk thread.MethodStates.Count frame
