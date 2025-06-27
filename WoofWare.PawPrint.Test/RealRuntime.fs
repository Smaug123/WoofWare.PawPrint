namespace WoofWare.PawPrint.Test

/// Result of executing the program using the real .NET runtime.
type RealRuntimeResult =
    {
        ExitCode : int
    }

[<RequireQualifiedAccess>]
module RealRuntime =
    /// Execute an assembly using the real .NET runtime and capture the result.
    let executeWithRealRuntime (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        let assy = System.Reflection.Assembly.Load assemblyBytes
        let result = assy.EntryPoint.Invoke ((null : obj), [| args |]) |> unbox<int>

        {
            ExitCode = result
        }
