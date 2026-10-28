namespace WoofWare.PawPrint

/// The static methods of CoreLib's `System.Threading.Monitor` that CoreCLR's JIT calls around the
/// body of a synchronized method (`MethodImplAttributes.Synchronized`) on a target with funclets:
/// the ones `jithelpers.h` binds `CORINFO_HELP_MON_ENTER` and `CORINFO_HELP_MON_EXIT` to. Each
/// takes the lock and a `ref bool` saying whether it is held, which the JIT starts false.
type SynchronizedMethodHelpers =
    {
        /// The method run before the body, which takes the lock and sets the flag.
        Enter : string
        /// The method run after the body, however it ends, which releases the lock if the flag is set.
        Exit : string
    }

[<RequireQualifiedAccess>]
module SynchronizedMethodHelpers =

    /// The helpers `runtime`'s JIT calls.
    let onRuntime (runtime : EmulatedRuntime) : SynchronizedMethodHelpers =
        match runtime with
        | EmulatedRuntime.Net10 ->
            {
                Enter = "Enter"
                Exit = "ExitIfLockTaken"
            }
        | EmulatedRuntime.Net11 ->
            {
                Enter = "SynchronizedMethodEnter"
                Exit = "SynchronizedMethodExit"
            }
