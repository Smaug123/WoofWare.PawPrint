using System;

// Exercises the SystemNative_GetCpuUtilization PawPrint handler indirectly,
// through the public BCL surface that calls it.
//
// Unlike SystemNativeGetUnixRelease.cs, this does not hand-roll a direct
// [DllImport] onto the entry point. Interop.Sys.GetCpuUtilization is reached
// identically from Environment.UnixOrBrowser.cs and AppDomain.Unix.cs on
// every Unix flavour PawPrint models (there is no macOS-specific override the
// way Environment.OSVersion has via Interop.libobjc), so the public
// AppDomain.MonitoringTotalProcessorTime property already exercises the same
// code path a hand-rolled stub would, without needing to guess at the
// unmanaged ProcessCpuInformation struct's layout from guest code.
//
// This is a *pure* test, so it runs on the real CLR as well as under
// PawPrint. Actual CPU-time-consumed values are therefore not assertable:
// the host reports genuine, non-deterministic process CPU time, while
// PawPrint reports its own deterministic substitute (see NativeSystemNative.fs
// for why -- summary: nothing in the interpreter currently models per-process
// CPU consumption, so inventing a number would have no ground truth, whereas
// reporting "no CPU time recorded" is honest about that gap). What is
// asserted is the contract that holds on both: readings are never negative,
// and -- because process CPU time is a cumulative counter the kernel only
// ever adds to -- a later reading is never smaller than an earlier one. Under
// PawPrint both readings are identically zero, which trivially satisfies both
// properties.
class Program
{
    static int Main(string[] args)
    {
        TimeSpan first = AppDomain.CurrentDomain.MonitoringTotalProcessorTime;
        TimeSpan second = AppDomain.CurrentDomain.MonitoringTotalProcessorTime;

        if (first < TimeSpan.Zero) return 1;
        if (second < TimeSpan.Zero) return 2;
        if (second < first) return 3;

        return 0;
    }
}
