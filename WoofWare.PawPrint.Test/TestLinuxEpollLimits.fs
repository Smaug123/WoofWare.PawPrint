namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `LinuxEpollLimits` is two literals, and they are not independent of each other: the cap is
/// `INT_MAX / sizeof(struct epoll_event)` (fs/eventpoll.c). Checking the arithmetic rather than
/// the numbers is what makes an edit to one of them fail here instead of silently leaving a cap
/// derived from the other architecture's packing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLinuxEpollLimits =

    [<Test>]
    let ``the event cap is INT_MAX divided by the element size`` () : unit =
        LinuxEpollLimits.MaxEvents
        |> shouldEqual (Int32.MaxValue / LinuxEpollLimits.EventSize)

    /// The property `SystemNative_WaitForSocketEvents` relies on: every count the cap admits has
    /// a byte extent inside `int32`, so the multiplication its buffer screen performs cannot
    /// overflow — which is why that screen must sit *behind* the cap and not in front of it.
    ///
    /// The second half is what stops this being satisfiable by any cap at all: one past the cap
    /// really does overflow, so the bound is tight rather than merely sufficient.
    [<Test>]
    let ``the cap is exactly the largest count whose byte extent fits in int32`` () : unit =
        let extentOf (count : int) : int64 =
            int64 count * int64 LinuxEpollLimits.EventSize

        extentOf LinuxEpollLimits.MaxEvents <= int64 Int32.MaxValue |> shouldEqual true

        extentOf (LinuxEpollLimits.MaxEvents + 1) > int64 Int32.MaxValue
        |> shouldEqual true

    /// Pins the packing, and so the architecture, as a fact rather than as a comment: 12 is
    /// `{ __poll_t events; __u64 data; }` under `__attribute__((packed))`, which is what
    /// `linux/eventpoll.h` applies under `#ifdef __x86_64__`. The unpacked struct — every other
    /// architecture, aarch64 included — is 16.
    [<Test>]
    let ``the element size is the x86-64 packed struct`` () : unit =
        LinuxEpollLimits.EventSize |> shouldEqual (sizeof<uint32> + sizeof<uint64>)
