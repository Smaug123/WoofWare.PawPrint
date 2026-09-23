using System;

// What CoreCLR's configuration knobs make of an environment holding an empty
// entry. The runtime reads a `DOTNET_`/`COMPlus_` knob through CLRConfig, which
// at startup walks the `GetEnvironmentStringsW` block to record which knob
// names exist (`CLRConfig::Initialize`, coreclr/utilcode/clrconfig.cpp), and
// later answers "not set" for any knob that walk did not record. The walk stops
// at the first empty string in the block, and an empty environment entry is
// exactly that, so a knob set after one is invisible to CLRConfig -- while the
// PAL's own lookup, which `Environment.GetEnvironmentVariable` uses, still
// finds it, and CoreLib's `GetEnvironmentVariables` walk stops at the same
// place CLRConfig's does.
//
// Impure because no oracle process can be started with an empty `envp` entry
// through `ProcessStartInfo`. The expected exit codes were measured instead,
// by `execve`-ing the real runtime with a hand-built `envp` holding these
// entries (with `HOME=/tmp` ahead of them), on Darwin 25.6 (.NET 10.0.7) and
// Linux 6.18.5 (.NET 10.0.11): each run reported the `ProcessorCount` its
// registration in TestImpureCases expects.
//
// Returns `Environment.ProcessorCount` after checking the two facts that hold
// for every registration: the variable after the empty entry is visible to the
// PAL lookup and absent from the enumeration.
public class TestClrConfigCacheStopsAtEmptyEntry
{
    public static int Main(string[] argv)
    {
        if (Environment.GetEnvironmentVariable("DOTNET_PROCESSOR_COUNT") != "5") return 100;
        if (Environment.GetEnvironmentVariables().Contains("DOTNET_PROCESSOR_COUNT")) return 101;
        return Environment.ProcessorCount;
    }
}
