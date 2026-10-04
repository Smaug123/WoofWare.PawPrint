namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework

/// The x64 JIT of .NET runtimes 10.0.0 to 10.0.11 sends every input of a 64-entry IL `switch`
/// with two case targets to the wrong target, whenever entries 32 to 63 all go where entry 0 goes
/// (dotnet/runtime#131716, fixed in 10.0.12). An F# match with two outcomes on a union of exactly
/// 64 cases compiles to that shape, and `UnixSystemDefect` has 64 cases. Other architectures are
/// unaffected, so a pass on an arm64 machine says nothing about CI's x64 runners.
///
/// Every test in this host runs on the same runtime, so this fails in one place, naming the
/// cause, rather than as unrelated-looking failures wherever such a match happens to be.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestJitSwitchLowering =

    /// Exactly 64 cases, so a match on it switches over 64 tags.
    type private SixtyFour =
        | Case00
        | Case01
        | Case02
        | Case03
        | Case04
        | Case05
        | Case06
        | Case07
        | Case08
        | Case09
        | Case10
        | Case11
        | Case12
        | Case13
        | Case14
        | Case15
        | Case16
        | Case17
        | Case18
        | Case19
        | Case20
        | Case21
        | Case22
        | Case23
        | Case24
        | Case25
        | Case26
        | Case27
        | Case28
        | Case29
        | Case30
        | Case31
        | Case32
        | Case33
        | Case34
        | Case35
        | Case36
        | Case37
        | Case38
        | Case39
        | Case40
        | Case41
        | Case42
        | Case43
        | Case44
        | Case45
        | Case46
        | Case47
        | Case48
        | Case49
        | Case50
        | Case51
        | Case52
        | Case53
        | Case54
        | Case55
        | Case56
        | Case57
        | Case58
        | Case59
        | Case60
        | Case61
        | Case62
        | Case63

    /// Compiles to a 64-entry switch with two targets: three tags go to `true`, and the rest,
    /// including every tag from 32 to 63, go where tag 0 goes.
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private isSpecial (value : SixtyFour) : bool =
        match value with
        | SixtyFour.Case14
        | SixtyFour.Case25
        | SixtyFour.Case26 -> true
        | _ -> false

    let private specialTags : Set<int> = Set.ofList [ 14 ; 25 ; 26 ]

    [<Test>]
    let ``isSpecial compiles to a 64-entry switch`` () : unit =
        // The canary below only means something while the F# compiler emits this shape.
        let methodInfo =
            typeof<SixtyFour>.DeclaringType
                .GetMethod ("isSpecial", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)

        let il = methodInfo.GetMethodBody().GetILAsByteArray ()

        // 0x45 is `switch`, followed by its entry count as an int32. A stray 0x45 inside another
        // instruction's operand could only add entries to this list, never remove the real one.
        [
            for i in 0 .. il.Length - 5 do
                if il.[i] = 0x45uy then
                    yield BitConverter.ToInt32 (il, i + 1)
        ]
        |> shouldContain 64

    [<Test>]
    let ``a 64-entry switch with two targets branches correctly`` () : unit =
        let cases = FSharpType.GetUnionCases (typeof<SixtyFour>, true)
        cases.Length |> shouldEqual 64

        let wrong =
            cases
            |> Array.filter (fun case ->
                let value = FSharpValue.MakeUnion (case, [||], true) :?> SixtyFour
                isSpecial value <> Set.contains case.Tag specialTags
            )
            |> Array.map (fun case -> case.Name)

        if wrong.Length > 0 then
            let names = String.concat ", " wrong

            failwith
                $"This test host's JIT miscompiles a 64-entry switch with two targets: %d{wrong.Length} of 64 inputs took the wrong branch (%s{names}). This is dotnet/runtime#131716, in the x64 JIT of .NET runtimes 10.0.0 to 10.0.11: the runtime must be 10.0.12 or later. This host runs %O{Environment.Version} on %O{RuntimeInformation.ProcessArchitecture}."
