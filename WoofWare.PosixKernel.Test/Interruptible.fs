namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// The wake condition of a park, as a test that knows what the syscall itself
/// waits for states it.
[<RequireQualifiedAccess>]
module Interruptible =

    /// What a task parked in a syscall that waits for `own` waits for: `own`, or
    /// a signal with a handler, which every park here can be ended by.
    let condition (own : WakeCondition) : WakeCondition =
        let signal = WakeCondition.Primitive WakePrimitive.SignalDeliverable

        match own with
        | WakeCondition.Primitive _ -> WakeCondition.AnyOf (own, [ signal ])
        | WakeCondition.AnyOf (first, rest) -> WakeCondition.AnyOf (first, rest @ [ signal ])
