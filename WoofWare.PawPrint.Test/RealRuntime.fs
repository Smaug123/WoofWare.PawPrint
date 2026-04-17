namespace WoofWare.PawPrint.Test

/// Result of executing the program using the real .NET runtime.
type RealRuntimeResult =
    /// The program returned normally with an exit code.
    | NormalExit of exitCode : int
    /// The program threw an unhandled exception.
    | UnhandledException of exn : System.Exception

[<RequireQualifiedAccess>]
module RealRuntime =
    /// Execute an assembly using the real .NET runtime and capture the result.
    let executeWithRealRuntime (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        let assy = System.Reflection.Assembly.Load assemblyBytes

        try
            let result = assy.EntryPoint.Invoke ((null : obj), [| args |]) |> unbox<int>
            RealRuntimeResult.NormalExit result
        with :? System.Reflection.TargetInvocationException as tie ->
            RealRuntimeResult.UnhandledException (tie.InnerException |> Option.ofObj |> Option.defaultValue (tie :> _))
