namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// The escape analysis over a fixture whose every method states what must, and must not, escape
/// it. Each expectation was written from the analysis's stated envelope before it was run.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEscapeAnalysis =

    let private source =
        """
using System;

namespace Fixture;

public class Locking
{
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public void Instance() { }
}

public static class Intrinsics
{
    // `Unsafe.As<TFrom, TTo>(ref TFrom)` is a body CoreCLR's VM substitutes; the read through the
    // result is the caller's own.
    public static uint Reinterpret(ref int x) => System.Runtime.CompilerServices.Unsafe.As<int, uint>(ref x);

    // A barrier the JIT emits in place of the method's call to itself.
    public static void Fence() => System.Threading.Interlocked.MemoryBarrier();

    // A capability query, which the JIT answers as a constant for the CPU.
    public static bool Accelerated() => System.Runtime.Intrinsics.Vector128.IsHardwareAccelerated;
}

public static class Cases
{
    public static void ThrowsDirectly() { throw new InvalidOperationException("boom"); }

    public static void CaughtExactly()
    {
        try { ThrowsDirectly(); }
        catch (InvalidOperationException) { }
    }

    // SystemException is CoreLib's, so seeing that it covers InvalidOperationException needs the
    // base chain of a type in another assembly.
    public static void CaughtByBase()
    {
        try { ThrowsDirectly(); }
        catch (SystemException) { }
    }

    public static void UnrelatedCatch()
    {
        try { ThrowsDirectly(); }
        catch (ArgumentException) { }
    }

    public static void FinallyDoesNotCatch()
    {
        try { ThrowsDirectly(); }
        finally { GC.KeepAlive(null); }
    }

    public static void PropagatesOneHop() { UnrelatedCatch(); }

    // Entering it takes a monitor, which an interrupted wait abandons before any IL runs.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public static void Synchronised() { }

    public static void Unsynchronised() { }

    public static void CatchesInterruption()
    {
        try { Synchronised(); }
        catch (System.Threading.ThreadInterruptedException) { }
    }

    // Its body releases the monitor itself, so releasing it again on the way out throws, past
    // the body's handlers.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public static void ReleasesEarly()
    {
        try { System.Threading.Monitor.Exit(typeof(Cases)); }
        catch (Exception) { }
    }

    public static void TwoSources(bool b)
    {
        if (b) { ThrowsDirectly(); }
        else { throw new FormatException(); }
    }

    public static void CatchesBoth(bool b)
    {
        try { TwoSources(b); }
        catch (Exception) { }
    }

    public static int Leaf(int a) => a;

    public static void Recursive(int n)
    {
        if (n <= 0) { ThrowsDirectly(); }
        else { Recursive(n - 1); }
    }

    public static void Rethrows()
    {
        try { ThrowsDirectly(); }
        catch (Exception) { throw; }
    }

    public static void ThrowsLocalDerived() { throw new Derived(); }

    public static void CaughtByLocalBase()
    {
        try { ThrowsLocalDerived(); }
        catch (LocalBase) { }
    }

    static InvalidOperationException MakeInvalid() => new InvalidOperationException("made");

    // Only the helper's declared return type is known of what is thrown.
    public static void ThrowsHelperResult() { throw MakeInvalid(); }

    public static void HelperCaughtByBase()
    {
        try { ThrowsHelperResult(); }
        catch (SystemException) { }
    }

    // ObjectDisposedException derives from InvalidOperationException, so it catches only some of
    // what the helper may return.
    public static void HelperNotCaughtBySubclass()
    {
        try { ThrowsHelperResult(); }
        catch (ObjectDisposedException) { }
    }

    public static string CallsVirtual(Animal a) => a.Speak();

    // C# calls an instance method with callvirt; a non-virtual target is not dispatched.
    public static void CallsNonVirtualViaCallvirt(Sealed s) { s.Boom(); }

    // Cases has no type initializer, so calling into it cannot fail on one.
    public static int CallsLeaf() => Leaf(1);

    public static void CoreLibThrowHelper(object o) { ArgumentNullException.ThrowIfNull(o); }

    // Nothing says what the parameter is at run time.
    public static void ThrowsParameter(Exception e) { throw e; }
}

// A static virtual is dispatched on the type argument: the default body is not what runs for
// ThrowingParse.
public interface IParse
{
    static virtual int Parse() => 1;
}

public sealed class ThrowingParse : IParse
{
    public static int Parse() => throw new FormatException();
}

public static class StaticVirtualCases
{
    public static int CallParse<T>() where T : IParse => T.Parse();
}

// Each instantiation has its own initializer: initializing G<string> runs G<int>'s, which can fail.
public class G<T>
{
    public static int Value = 1 / G<int>.Value;
}

public class LocalBase : Exception { }

public class Derived : LocalBase { }

public class Animal
{
    public virtual string Speak() => "...";
}

public sealed class Sealed
{
    public void Boom() { throw new FormatException(); }
}

public static class Boom
{
    public static readonly int Value = int.Parse("not a number");

    public static int M() => Value;
}

public static class CctorCases
{
    public static int CallsBoom() => Boom.M();
}

public class Shadowed
{
    public int Field;
}

public static class ShadowCases
{
    // A catch for a locally declared System.NullReferenceException must not absorb the one the
    // runtime raises.
    public static int DereferencesNull(Shadowed s)
    {
        try { return s.Field; }
        catch (System.NullReferenceException) { return -1; }
    }
}
"""

    /// The local shadow the last case catches.
    let private shadow =
        """
namespace System;

public class NullReferenceException : Exception { }
"""

    /// What a fixture method's answer must hold. A thrown type is written `=T` for `Exactly T` and
    /// `<:T` for `SubtypeOf T`.
    type private Expectation =
        {
            Method : string * string
            Contains : string list
            Excludes : string list
            Unknown : bool option
        }

    let private expect (ty : string) (name : string) : Expectation =
        {
            Method = ty, name
            Contains = []
            Excludes = []
            Unknown = None
        }

    let private expectations : Expectation list =
        let ioe = "=System.InvalidOperationException"

        [
            { expect "Fixture.Cases" "ThrowsDirectly" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "CaughtExactly" with
                Excludes = [ ioe ]
            }
            { expect "Fixture.Cases" "CaughtByBase" with
                Excludes = [ ioe ]
            }
            { expect "Fixture.Cases" "UnrelatedCatch" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "FinallyDoesNotCatch" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "PropagatesOneHop" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "Synchronised" with
                Contains =
                    [
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
                // Its monitor is its type's, never null.
                Excludes = [ "=System.ArgumentNullException" ]
            }
            { expect "Fixture.Cases" "ReleasesEarly" with
                Contains = [ "=System.Threading.SynchronizationLockException" ]
            }
            // Reached by `call` on a null receiver, it locks on null.
            { expect "Fixture.Locking" "Instance" with
                Contains =
                    [
                        "=System.ArgumentNullException"
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
            }
            { expect "Fixture.Cases" "Unsynchronised" with
                Excludes =
                    [
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                        "=System.ArgumentNullException"
                    ]
            }
            { expect "Fixture.Cases" "CatchesInterruption" with
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
            }
            { expect "Fixture.Cases" "TwoSources" with
                Contains = [ ioe ; "=System.FormatException" ]
            }
            // `catch (Exception)` absorbs everything, including what could not be named.
            { expect "Fixture.Cases" "CatchesBoth" with
                Excludes = [ ioe ; "=System.FormatException" ; "=System.StackOverflowException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "Leaf" with
                Unknown = Some false
            }
            { expect "Fixture.Cases" "Recursive" with
                Contains = [ ioe ]
            }
            // The `throw;` is in the handler, outside the region its catch protects.
            { expect "Fixture.Cases" "Rethrows" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "ThrowsLocalDerived" with
                Contains = [ "=Fixture.Derived" ]
            }
            { expect "Fixture.Cases" "CaughtByLocalBase" with
                Excludes = [ "=Fixture.Derived" ]
            }
            { expect "Fixture.Cases" "ThrowsHelperResult" with
                Contains = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "HelperCaughtByBase" with
                Excludes = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "HelperNotCaughtBySubclass" with
                Contains = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "CallsVirtual" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "CallsNonVirtualViaCallvirt" with
                Contains = [ "=System.FormatException" ]
            }
            { expect "Fixture.Cases" "CallsLeaf" with
                Excludes = [ "=System.TypeInitializationException" ]
            }
            { expect "Fixture.Cases" "ThrowsParameter" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "CoreLibThrowHelper" with
                Contains = [ "=System.ArgumentNullException" ]
            }
            { expect "Fixture.StaticVirtualCases" "CallParse" with
                Unknown = Some true
            }
            { expect "Fixture.G`1" ".cctor" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            // `Boom`'s initializer fails, and calling `M` runs it first.
            { expect "Fixture.CctorCases" "CallsBoom" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            { expect "Fixture.ShadowCases" "DereferencesNull" with
                Contains = [ "=System.NullReferenceException" ]
            }
            { expect "Fixture.Intrinsics" "Reinterpret" with
                Contains = [ "=System.NullReferenceException" ]
                Unknown = Some false
            }
            { expect "Fixture.Intrinsics" "Fence" with
                Unknown = Some false
            }
            { expect "Fixture.Intrinsics" "Accelerated" with
                Unknown = Some false
            }
        ]

    /// An answer as the expectations spell it: `=T` for `Exactly T`, `<:T` for `SubtypeOf T`.
    let private render (analysis : EscapeAnalysisState) (escapes : Escapes) : Set<string> =
        escapes.Types
        |> Seq.map (fun thrown ->
            match thrown with
            | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
            | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
        )
        |> Set.ofSeq

    /// The method of this name on the type of this full name, in `assembly`.
    let private methodNamed (assembly : DumpedAssembly) (typeName : string) (methodName : string) : MethodKey =
        assembly.Methods
        |> Seq.pick (fun (KeyValue (handle, method)) ->
            if
                method.Name = methodName
                && TypeInfo.fullName
                    (fun h -> assembly.TypeDefs.[h])
                    assembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get] = typeName
            then
                Some (MethodKey.make assembly handle)
            else
                None
        )

    /// The architecture the framework under test's JIT compiles for.
    let private hostTarget () : JitTarget =
        match Runtime.InteropServices.RuntimeInformation.ProcessArchitecture with
        | Runtime.InteropServices.Architecture.Arm64 -> JitTarget.Arm64
        | Runtime.InteropServices.Architecture.X64 -> JitTarget.X64
        | other -> failwith $"No JIT table describes the host's architecture, %O{other}"

    /// An analysis over `corelib` and `assemblies`, with `bind` applied to the load context, loading
    /// any other assembly from `runtimeDirs`, for the JIT compiling for `target` on a CPU `profile`
    /// describes.
    let private analysisOf
        (corelib : DumpedAssembly)
        (runtimeDirs : string seq)
        (target : JitTarget)
        (profile : HardwareIntrinsicsProfile)
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies (corelib :: assemblies) |> bind

        EscapeAnalysis.create
            loggerFactory
            runtimeDirs
            target
            profile
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

    let private hostCoreLib () : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        Assembly.readFile
            loggerFactory
            (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

    /// An analysis over the host's CoreLib and `assemblies`, with `bind` applied to the load
    /// context, for the host's JIT on a CPU `profile` describes.
    let private analysisUnder
        (profile : HardwareIntrinsicsProfile)
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        analysisOf (hostCoreLib ()) (FrameworkUnderTest.runtimeDirs ()) (hostTarget ()) profile assemblies bind

    /// An analysis over CoreLib and `assemblies`, with `bind` applied to the load context, on a CPU
    /// with no instruction sets.
    let private analysisOver
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        analysisUnder HardwareIntrinsicsProfile.ScalarOnly assemblies bind

    [<Test>]
    let ``each fixture method's escaping exceptions are as stated`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let mutable analysis = analysisOver [ fixture ] id
        let failures = ResizeArray<string> ()

        for expectation in expectations do
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture (fst expectation.Method) (snd expectation.Method))

            analysis <- next
            let shown = render analysis escapes

            let describe () =
                let ty, name = expectation.Method
                $"%s{ty}::%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

            for wanted in expectation.Contains do
                if not (shown.Contains wanted) then
                    failures.Add $"%s{describe ()} lacks %s{wanted}"

            for unwanted in expectation.Excludes do
                if shown.Contains unwanted then
                    failures.Add $"%s{describe ()} has %s{unwanted}"

            match expectation.Unknown with
            | Some unknown when unknown <> escapes.Unknown ->
                failures.Add $"%s{describe ()}, expected unknown %b{unknown}"
            | _ -> ()

        if failures.Count > 0 then
            failures |> String.concat Environment.NewLine |> failwith

    /// The CoreLibs the CoreLib-wide tests read, the host's and the pinned linux-x64 one, each with
    /// the CPUs they answer for: one with no instruction sets, and one with every instruction set
    /// whose class in that CoreLib is a placeholder.
    let coreLibsAndProfiles : TestCaseData list =
        [
            for coreLib in [ "host" ; "linux-x64" ] do
                for profile in [ "scalar-only" ; "every instruction set" ] do
                    TestCaseData(coreLib, profile).SetArgDisplayNames ($"%s{coreLib} CoreLib", $"%s{profile} CPU")
        ]

    /// A CoreLib the CoreLib-wide tests read, where to load the rest of its framework from, and the
    /// JIT target it was built for.
    let private coreLibNamed (name : string) : DumpedAssembly * string list * JitTarget =
        match name with
        | "host" -> hostCoreLib (), List.ofSeq (FrameworkUnderTest.runtimeDirs ()), hostTarget ()
        | "linux-x64" ->
            match Environment.GetEnvironmentVariable "DOTNET_LINUX_FRAMEWORK_DIR" with
            | null
            | "" ->
                Assert.Ignore "DOTNET_LINUX_FRAMEWORK_DIR is unset; run inside the Nix devshell"
                failwith "unreachable: Assert.Ignore did not throw"
            | dir ->
                let _, loggerFactory = LoggerFactory.makeTest ()

                Assembly.readFile loggerFactory (Path.Combine (dir, "System.Private.CoreLib.dll")),
                [ dir ],
                JitTarget.X64
        | other -> failwith $"unknown CoreLib %s{other}"

    /// Whether `method`'s IL calls, constructs and throws nothing but through its calls to itself, so
    /// that whatever else it raises is what its instructions raise by themselves.
    let private callsOnlyItself (corelib : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        match corelib.Methods.[method].Body with
        | MethodBody.Il body ->
            body.Instructions
            |> List.forall (fun (op, _) ->
                match op with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Calli | UnaryMetadataTokenIlOp.Jmp | UnaryMetadataTokenIlOp.Ldftn | UnaryMetadataTokenIlOp.Ldvirtftn),
                                           operand) ->
                    match operand with
                    | MetadataOperand.FromMetadata token -> token.Token = MetadataToken.MethodDef method
                    | _ -> false
                | IlOp.Nullary NullaryIlOp.Throw
                | IlOp.Nullary NullaryIlOp.Rethrow -> false
                | _ -> true
            )
        | _ -> false

    let private profileNamed (corelib : DumpedAssembly) (name : string) : HardwareIntrinsicsProfile =
        match name with
        | "scalar-only" -> HardwareIntrinsicsProfile.ScalarOnly
        | "every instruction set" ->
            let expansions =
                [
                    for KeyValue (handle, _) in corelib.Methods do
                        if IntrinsicBody.isIntrinsic corelib handle then
                            match IntrinsicBody.classify corelib handle with
                            | IntrinsicBody.JitExpansion expansion -> yield expansion
                            | _ -> ()
                ]

            {
                IsSupported =
                    expansions
                    |> List.choose (fun expansion ->
                        match expansion with
                        | JitExpansion.IsSupportedQuery c
                        | JitExpansion.HardwareInstruction c -> Some c
                        | _ -> None
                    )
                    |> Set.ofList
                IsHardwareAccelerated =
                    expansions
                    |> List.choose (fun expansion ->
                        match expansion with
                        | JitExpansion.IsHardwareAcceleratedQuery c -> Some c
                        | _ -> None
                    )
                    |> Set.ofList
            }
        | other -> failwith $"unknown profile %s{other}"

    /// Every intrinsic in CoreLib, answered from what CoreCLR runs for it: the IL its VM substitutes
    /// where there is some, and otherwise its own IL with its call to itself performed as the JIT
    /// expands it for the CPU: a capability query answers a constant, an instruction the CPU lacks
    /// throws `PlatformNotSupportedException`, and one it has raises what the JIT's tables say.
    [<TestCaseSource(nameof coreLibsAndProfiles)>]
    let ``each CoreLib intrinsic is analysed as what CoreCLR runs for it``
        (coreLibName : string)
        (profileName : string)
        : unit
        =
        let corelib, runtimeDirs, target = coreLibNamed coreLibName
        let profile = profileNamed corelib profileName
        let mutable analysis = analysisOf corelib runtimeDirs target profile [] id
        let failures = ResizeArray<string> ()
        let mutable substituted = 0
        let mutable primitives = 0
        let mutable placeholders = 0
        let mutable constants = 0
        let mutable unsupported = 0
        let mutable instructions = 0
        let mutable bareInstructions = 0

        let rangeHelperEscapes, rangeHelperShown =
            let next, escapes =
                EscapeAnalysis.escapes
                    analysis
                    (MethodKey.make corelib (IntrinsicBody.argumentOutOfRangeHelper corelib))

            analysis <- next
            let shown = render analysis escapes

            // The helper constructs and throws the exception, and constructing it can exhaust memory.
            for wanted in [ "=System.ArgumentOutOfRangeException" ; "=System.OutOfMemoryException" ] do
                shown |> shouldContain wanted

            escapes, shown

        let instructionFaultName (fault : InstructionFault) : string =
            match fault with
            | InstructionFault.NullAddress -> "=System.NullReferenceException"
            | InstructionFault.ImmediateOutOfRange -> "=System.ArgumentOutOfRangeException"
            | InstructionFault.ZeroDivisor -> "=System.DivideByZeroException"
            | InstructionFault.QuotientOverflow -> "=System.OverflowException"

        let mutable wholePrimitives = 0

        let faultName (fault : PrimitiveFault) : string =
            match fault with
            | PrimitiveFault.NullReference -> "=System.NullReferenceException"
            | PrimitiveFault.DataMisaligned -> "=System.DataMisalignedException"

        for KeyValue (handle, method) in corelib.Methods do
            if IntrinsicBody.isIntrinsic corelib handle then
                let next, escapes = EscapeAnalysis.escapes analysis (MethodKey.make corelib handle)
                analysis <- next
                let shown = render analysis escapes

                let describe () =
                    $"%s{method.RequiredDeclaringType.Name}::%s{method.Name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

                match VmSubstitution.unsafeStub corelib handle, IntrinsicBody.classify corelib handle with
                | Some _, _ ->
                    substituted <- substituted + 1

                    if escapes.Unknown then
                        failures.Add $"%s{describe ()}: the VM's IL for it is all there is to see"
                | None, IntrinsicBody.JitExpansion (JitExpansion.Primitive primitive) ->
                    primitives <- primitives + 1

                    for fault, _ in (IntrinsicPrimitive.contract primitive).Raises do
                        if not (shown.Contains (faultName fault)) then
                            failures.Add $"%s{describe ()} lacks %s{faultName fault}, which %A{primitive} can raise"
                | None, IntrinsicBody.VmSubstitution ->
                    // The VM chooses the substitute by instantiation; where it is a primitive, that
                    // is the whole method.
                    match IntrinsicPrimitive.recognise corelib handle with
                    | Some primitive ->
                        wholePrimitives <- wholePrimitives + 1

                        let wanted =
                            (IntrinsicPrimitive.contract primitive).Raises
                            |> List.map (fst >> faultName)
                            |> Set.ofList

                        if escapes.Unknown || shown <> wanted then
                            failures.Add $"%s{describe ()}: %A{primitive} raises exactly %A{Set.toList wanted}"
                    | None -> ()
                | None, IntrinsicBody.JitExpansion expansion ->
                    placeholders <- placeholders + 1

                    match IntrinsicBody.expandSelfCall profile expansion with
                    | SelfCallExpansion.Constant _ ->
                        constants <- constants + 1

                        if escapes.Unknown then
                            failures.Add $"%s{describe ()}: a capability query is a constant"
                    | SelfCallExpansion.ThrowPlatformNotSupported ->
                        unsupported <- unsupported + 1

                        // The JIT calls CoreLib's throw helper, whose `newobj` can exhaust memory.
                        for wanted in [ "=System.PlatformNotSupportedException" ; "=System.OutOfMemoryException" ] do
                            if not (shown.Contains wanted) then
                                failures.Add $"%s{describe ()} lacks %s{wanted}: the CPU lacks the instruction"
                    | SelfCallExpansion.HardwareInstruction intrinsicClass ->
                        match HardwareInstruction.contract target intrinsicClass method.Name with
                        | InstructionContract.Unknown ->
                            if not escapes.Unknown then
                                failures.Add $"%s{describe ()}: the JIT's tables do not say what it raises"
                        | InstructionContract.Raises faults ->
                            instructions <- instructions + 1

                            // An out-of-range immediate is thrown by a call to CoreLib's helper,
                            // so the instruction raises what the helper does.
                            let viaHelper = faults.Contains InstructionFault.ImmediateOutOfRange

                            // A body that does more than call itself, such as `Avx2.GatherVector128`
                            // constructing the exception for a `scale` it rejects, may be unknown
                            // for that.
                            if callsOnlyItself corelib handle then
                                bareInstructions <- bareInstructions + 1
                                let expected = viaHelper && rangeHelperEscapes.Unknown

                                if escapes.Unknown <> expected then
                                    failures.Add
                                        $"%s{describe ()}: the JIT's tables say it raises %A{faults}, so unknown should be %b{expected}"

                            let wanted =
                                faults
                                |> Seq.filter (fun fault -> fault <> InstructionFault.ImmediateOutOfRange)
                                |> Seq.map instructionFaultName
                                |> Set.ofSeq
                                |> Set.union (if viaHelper then rangeHelperShown else Set.empty)

                            for wanted in wanted do
                                if not (shown.Contains wanted) then
                                    failures.Add $"%s{describe ()} lacks %s{wanted}"
                    // Answered by the branch above.
                    | SelfCallExpansion.Primitive _ -> ()
                    | SelfCallExpansion.Unrecognised ->
                        if not escapes.Unknown then
                            failures.Add $"%s{describe ()}: what the JIT emits is not known"
                | None, _ -> ()

        if failures.Count > 0 then
            failures |> Seq.truncate 40 |> String.concat Environment.NewLine |> failwith

        substituted |> shouldBeGreaterThan 20
        primitives |> shouldBeGreaterThan 10
        placeholders |> shouldBeGreaterThan 20
        wholePrimitives |> shouldBeGreaterThan 0
        constants |> shouldBeGreaterThan 20

        match profileName with
        | "scalar-only" ->
            unsupported |> shouldBeGreaterThan 1000
            instructions |> shouldEqual 0
        | _ ->
            unsupported |> shouldEqual 0
            instructions |> shouldBeGreaterThan 1000
            bareInstructions |> shouldBeGreaterThan 1000

    [<Test>]
    let ``a profile supporting another architecture's instruction set is refused`` () : unit =
        let foreign = JitTarget.all |> List.find (fun target -> target <> hostTarget ())

        let profile =
            { HardwareIntrinsicsProfile.ScalarOnly with
                IsSupported =
                    Set.singleton
                        {
                            Namespace = JitTarget.instructionSetNamespace foreign
                            Path = [ "Aes" ]
                        }
            }

        Assert.Throws<ArgumentException> (fun () -> analysisUnder profile [] id |> ignore<EscapeAnalysisState>)
        |> ignore<ArgumentException>

    /// A client compiled against one version of a provider, run against another that lacks what it
    /// uses. The JIT binds a body's tokens before the body runs, so the failure comes out of the
    /// caller and the client's own `catch` cannot stop it.
    [<Test>]
    let ``what the provider no longer has fails to bind, past the method's own handlers`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let version1 =
            """
namespace Provider;

public class Parent
{
    public static int Value = 1;
    public static void Gone() { }
    public static void Varargs(__arglist) { }
}

public class GoneType { }

public class GoneException : System.Exception { }

public struct GoneStruct { }
"""

        let version2 =
            """
namespace Provider;

public class Parent
{
    public static void Varargs(__arglist) { }
}
"""

        let client =
            """
namespace Client;

public static class Uses
{
    public static int Read() => Provider.Parent.Value;
    public static void CallGone() { Provider.Parent.Gone(); }
    public static void CaughtCallGone()
    {
        try { Provider.Parent.Gone(); }
        catch (System.MissingMethodException) { }
    }
    public static object UseGoneType() => new Provider.GoneType();
    public static bool IsGone(object o) => o is Provider.GoneType;
    public static object ListOfGone() => new System.Collections.Generic.List<Provider.GoneType>();
    public static bool LocalOfGone()
    {
        Provider.GoneType x = null;
        return x != null;
    }
    public static int CatchGone(int x)
    {
        try { return 1 / x; }
        catch (Provider.GoneException) { return 42; }
    }
    static void Accept(Provider.GoneType x) { }
    public static void PassGone() { Accept(null); }
    public static unsafe int CallGoneIndirectly(delegate*<Provider.GoneStruct> f)
    {
        try { f(); return 0; }
        catch (System.Exception) { return 1; }
    }
    public static void PassGoneAsVararg() { Provider.Parent.Varargs(__arglist((Provider.GoneType)null)); }
    public static bool CaughtLocalOfGone()
    {
        try
        {
            Provider.GoneType x = null;
            return x != null;
        }
        catch (System.TypeLoadException) { return false; }
    }
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let providerImage1 = compile "Provider" [] version1
        let clientAssembly = read "Client" (compile "Client" [ providerImage1 ] client)

        let answers (provider : DumpedAssembly) : string -> Set<string> =
            let providerReference =
                clientAssembly.AssemblyReferences.Values
                |> Seq.find (fun r -> r.Name.Name = "Provider")

            let mutable analysis =
                analysisOver
                    [ clientAssembly ; provider ]
                    (fun loaded -> fst (loaded.WithBoundReference providerReference provider))

            fun methodName ->
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" methodName)

                analysis <- next
                render analysis escapes

        let against1 = answers (read "Provider" providerImage1)
        let against2 = answers (read "Provider" (compile "Provider" [] version2))

        for methodName, failure in
            [
                "Read", "=System.MissingFieldException"
                "CallGone", "=System.MissingMethodException"
                "CaughtCallGone", "=System.MissingMethodException"
                "UseGoneType", "=System.TypeLoadException"
                // A type token of its own.
                "IsGone", "=System.TypeLoadException"
                // A type argument of the member's parent, whose definition does resolve.
                "ListOfGone", "=System.TypeLoadException"
                // Named by no instruction, only by the type of a local.
                "LocalOfGone", "=System.TypeLoadException"
                "CaughtLocalOfGone", "=System.TypeLoadException"
                // Named only by a `catch` clause.
                "CatchGone", "=System.TypeLoadException"
                // Named only by the signature of the method called.
                "PassGone", "=System.TypeLoadException"
                // Named only by a vararg call site's extra arguments, which no definition declares.
                "PassGoneAsVararg", "=System.TypeLoadException"
                // Named only by an indirect call's signature.
                "CallGoneIndirectly", "=System.TypeLoadException"
            ] do
            let bound = against1 methodName
            let unbound = against2 methodName

            if bound.Contains failure then
                failwith $"%s{methodName} against the provider it was compiled against: %A{Set.toList bound}"

            if not (unbound.Contains failure) then
                failwith $"%s{methodName} against the provider lacking what it uses: %A{Set.toList unbound}"

    [<Test>]
    let ``a catch absorbs an exception however deep its base chain`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let depth = 300

        let classes =
            [ 1..depth ]
            |> List.map (fun i ->
                let parent = if i = 1 then "System.Exception" else $"E%d{i - 1}"
                $"public class E%d{i} : %s{parent} {{ }}"
            )
            |> String.concat "\n"

        let source =
            $"""
namespace Deep;

%s{classes}

public static class Throws
{{
    public static void Caught()
    {{
        try {{ throw new E%d{depth}(); }}
        catch (System.Exception) {{ }}
    }}
}}
"""

        let image =
            Roslyn.compileAssembly "Deep" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let assembly =
            Assembly.read loggerFactory (Some "Deep.dll") (new MemoryStream (image))

        let analysis = analysisOver [ assembly ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed assembly "Deep.Throws" "Caught")

        render analysis escapes |> shouldNotContain $"=Deep.E%d{depth}"

    [<Test>]
    let ``a module initializer runs when another module first binds to what it declares`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider =
            """
namespace Provider;

static class Init
{
    [System.Runtime.CompilerServices.ModuleInitializer]
    internal static void Run() => throw new System.InvalidOperationException();
}

public static class P
{
    public static int Field;
    public static void M() { }
    public static void CallsM() { M(); }
}

public class Base { }

public class Foreign { }
"""

        let client =
            """
namespace Client;

public class Derived : Provider.Base
{
    public static void Static() { }
}

public class Local<T> { }

public class DerivedOfForeign : Local<Provider.Foreign>
{
    public static void Static() { }
}

public static class Uses
{
    public static void CallM() { Provider.P.M(); }
    public static int CaughtCallM()
    {
        try { Provider.P.M(); return 0; }
        catch (System.TypeInitializationException) { return 1; }
    }
    public static int ReadField() => Provider.P.Field;
    public static object NewDerived() => new Derived();
    public static System.Type TypeOfDerived() => typeof(Derived);
    public static void CallDerivedStatic() { Derived.Static(); }
    public static void CallDerivedOfForeignStatic() { DerivedOfForeign.Static(); }
    public static int Local() => 1;
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let providerImage = compile "Provider" [] provider
        let clientImage = compile "Client" [ providerImage ] client

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let providerAssembly = read "Provider" providerImage
        let clientAssembly = read "Client" clientImage

        let providerReference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference providerReference providerAssembly))

        let reportsInitialisation (assembly : DumpedAssembly) (typeName : string) (methodName : string) : bool =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly typeName methodName)

            analysis <- next
            render analysis escapes |> Set.contains "=System.TypeInitializationException"

        // Each in a context of its own, since a module initializer that failed fails every later
        // binding the same way.
        let escapesOnRealRuntime (methodName : string) : bool =
            let context =
                new TestMethodReferenceResolution.ImagesContext (
                    Map.ofList [ "Provider", providerImage ; "Client", clientImage ]
                )

            try
                let uses = context.LoadFromAssemblyName(AssemblyName "Client").GetType "Client.Uses"

                try
                    uses.GetMethod(methodName).Invoke ((null : obj), Array.empty<obj>)
                    |> ignore<obj>

                    false
                with :? TargetInvocationException ->
                    true
            finally
                context.Unload ()

        for methodName in
            [
                "CallM"
                "CaughtCallM"
                "ReadField"
                "NewDerived"
                // Provider is reached only through Derived's base type.
                "TypeOfDerived"
                "CallDerivedStatic"
                // Provider is reached only through an argument of that base type.
                "CallDerivedOfForeignStatic"
            ] do
            escapesOnRealRuntime methodName |> shouldEqual true

            if not (reportsInitialisation clientAssembly "Client.Uses" methodName) then
                failwith $"%s{methodName} binds into Provider, whose module initializer throws"

        escapesOnRealRuntime "Local" |> shouldEqual false
        reportsInitialisation clientAssembly "Client.Uses" "Local" |> shouldEqual false
        // A module's own code runs only once its initializer has.
        reportsInitialisation providerAssembly "Provider.P" "CallsM"
        |> shouldEqual false

    /// `int32[,]`'s constructor taking lower bounds and lengths raises `ArgumentOutOfRangeException`
    /// for bounds whose upper end overflows; the one taking lengths alone does not. C# spells
    /// neither, so the calls are emitted directly.
    [<Test>]
    let ``a multidimensional array constructor taking lower bounds can reject them`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "ArrayCtors.dll",
            metadata.GetOrAddGuid (Guid "3c9d1b2a-4e5f-4a6b-9c8d-7e6f5a4b3c2d"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "ArrayCtors",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        let rankTwo =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .TypeSpecificationSignature()
                .Array (
                    (fun element -> element.Int32 ()),
                    (fun shape -> shape.Shape (2, ImmutableArray.Empty, ImmutableArray.Create (0, 0)))
                )

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)

        let constructor (arity : int) : MemberReferenceHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    arity,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters ->
                        for _ in 1..arity do
                            parameters.AddParameter().Type().Int32 ()
                    )
                )

            metadata.AddMemberReference (
                (TypeSpecificationHandle.op_Implicit rankTwo : EntityHandle),
                metadata.GetOrAddString ".ctor",
                metadata.GetOrAddBlob blob
            )

        let body (arity : int) : int =
            let code = InstructionEncoder (BlobBuilder ())

            for _ in 1..arity do
                code.LoadConstantI4 1

            code.OpCode ILOpCode.Newobj
            code.Token (MemberReferenceHandle.op_Implicit (constructor arity) : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let staticVoid =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let addMethod (name : string) (arity : int) =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                staticVoid,
                body arity,
                MetadataTokens.ParameterHandle 1
            )

        let lowerBounds = addMethod "LowerBounds" 4
        addMethod "Lengths" 2 |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            lowerBounds
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Arrays",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            lowerBounds
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        let assembly =
            Assembly.read loggerFactory (Some "ArrayCtors.dll") (new MemoryStream (image.ToArray ()))

        let mutable analysis = analysisOver [ assembly ] id

        let answer (name : string) =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly "W.Arrays" name)

            analysis <- next
            render analysis escapes

        let withBounds = answer "LowerBounds"
        let withoutBounds = answer "Lengths"
        withBounds |> shouldContain "=System.ArgumentOutOfRangeException"
        withBounds |> shouldContain "=System.OverflowException"
        withoutBounds |> shouldNotContain "=System.ArgumentOutOfRangeException"
        withoutBounds |> shouldContain "=System.OverflowException"

    /// `ldvirtftn` of an interface method, on an object implementing `IDynamicInterfaceCastable`,
    /// asks that object's `GetInterfaceImplementation`, which may throw anything; of a class's
    /// virtual method, it runs nothing. C# spells neither without a delegate constructor after it,
    /// so the instructions are emitted directly.
    [<Test>]
    let ``resolving an interface method's pointer may run the receiver's code`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Pointers.dll",
            metadata.GetOrAddGuid (Guid "8e2b4d6f-1a3c-4e5f-9b7d-2c4e6a8b0d1f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Pointers",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        // Type definitions are numbered in the order they are added: `<Module>`, `IFoo`, `Base`,
        // `Resolves`; methods likewise: `IFoo.M`, `Base.M`, then `Resolve` and `ResolveBase`.
        let interfaceType = MetadataTokens.TypeDefinitionHandle 2
        let baseType = MetadataTokens.TypeDefinitionHandle 3
        let interfaceMethod = MetadataTokens.MethodDefinitionHandle 1
        let baseMethod = MetadataTokens.MethodDefinitionHandle 2

        let instanceInt32 =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Type().Int32 ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let staticTaking (parameterType : TypeDefinitionHandle) =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters ->
                        parameters
                            .AddParameter()
                            .Type()
                            .Type ((TypeDefinitionHandle.op_Implicit parameterType : EntityHandle), false)
                    )
                )

            metadata.GetOrAddBlob blob

        let virtualMethod =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.HideBySig

        metadata.AddMethodDefinition (
            virtualMethod ||| MethodAttributes.Abstract,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "M",
            instanceInt32,
            -1,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        let returnZero =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadConstantI4 0
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        metadata.AddMethodDefinition (
            virtualMethod,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "M",
            instanceInt32,
            returnZero,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        let resolving (target : MethodDefinitionHandle) =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadArgument 0
            code.OpCode ILOpCode.Ldvirtftn
            code.Token (MethodDefinitionHandle.op_Implicit target : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        for name, parameterType, target in
            [
                "Resolve", interfaceType, interfaceMethod
                "ResolveBase", baseType, baseMethod
            ] do
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                staticTaking parameterType,
                resolving target,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            interfaceMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "IFoo",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            interfaceMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Base",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            baseMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Resolves",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 3
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        let assembly =
            Assembly.read loggerFactory (Some "Pointers.dll") (new MemoryStream (image.ToArray ()))

        let analysis = analysisOver [ assembly ] id

        let _, throughInterface =
            EscapeAnalysis.escapes analysis (methodNamed assembly "W.Resolves" "Resolve")

        let _, throughClass =
            EscapeAnalysis.escapes analysis (methodNamed assembly "W.Resolves" "ResolveBase")

        throughInterface.Unknown |> shouldEqual true
        throughClass.Unknown |> shouldEqual false

    [<Test>]
    let ``binding a member of a type variable turns on the instantiation, past the method's handlers`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Binds.dll",
            metadata.GetOrAddGuid (Guid "3b5d7f91-2a4c-4e6b-8d0f-1a3c5e7b9d2f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Binds",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )
            |> TypeReferenceHandle.op_Implicit

        // `!!0`, the method's own type parameter, as a MemberRef parent.
        let typeVariable : EntityHandle =
            let blob = BlobBuilder ()
            BlobEncoder(blob).TypeSpecificationSignature().GenericMethodTypeParameter 0
            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        let staticVoid (genericParameters : int) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(genericParameterCount = genericParameters)
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let valueField : EntityHandle =
            let blob = BlobBuilder ()
            BlobEncoder(blob).Field().Type().Int32 ()

            metadata.AddMemberReference (typeVariable, metadata.GetOrAddString "Value", metadata.GetOrAddBlob blob)
            |> MemberReferenceHandle.op_Implicit

        let methodM : EntityHandle =
            metadata.AddMemberReference (typeVariable, metadata.GetOrAddString "M", staticVoid 0)
            |> MemberReferenceHandle.op_Implicit

        // `try { <use> } catch (object) { }`: a catch-all around the only use of the member.
        let catchingAll (usesMember : InstructionEncoder -> unit) : int =
            let flow = ControlFlowBuilder ()
            let code = InstructionEncoder (BlobBuilder (), flow)
            let tryStart = code.DefineLabel ()
            let handlerStart = code.DefineLabel ()
            let handlerEnd = code.DefineLabel ()
            code.MarkLabel tryStart
            usesMember code
            code.Branch (ILOpCode.Leave_s, handlerEnd)
            code.MarkLabel handlerStart
            code.OpCode ILOpCode.Pop
            code.Branch (ILOpCode.Leave_s, handlerEnd)
            code.MarkLabel handlerEnd
            code.OpCode ILOpCode.Ret
            flow.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, objectRef)
            bodies.AddMethodBody code

        let readField =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "ReadField",
                staticVoid 1,
                catchingAll (fun code ->
                    code.OpCode ILOpCode.Ldsfld
                    code.Token valueField
                    code.OpCode ILOpCode.Pop
                ),
                MetadataTokens.ParameterHandle 1
            )

        let callMethod =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "CallMethod",
                staticVoid 1,
                catchingAll (fun code -> code.Call methodM),
                MetadataTokens.ParameterHandle 1
            )

        for method in [ readField ; callMethod ] do
            metadata.AddGenericParameter (
                (MethodDefinitionHandle.op_Implicit method : EntityHandle),
                GenericParameterAttributes.None,
                metadata.GetOrAddString "T",
                0
            )
            |> ignore<GenericParameterHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            readField
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Binds",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            readField
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image =
            let blob = BlobBuilder ()
            peBuilder.Serialize blob |> ignore<BlobContentId>
            blob.ToArray ()

        // The real runtime, at an instantiation lacking the member, shared (`object`) and not
        // (`int`): the binding failure gets past the catch-all.
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Binds", isCollectible = true)

        try
            let binds = context.LoadFromStream(new MemoryStream (image)).GetType "W.Binds"

            for name, missing in
                [
                    "ReadField", typeof<MissingFieldException>
                    "CallMethod", typeof<MissingMethodException>
                ] do
                for instantiation in [ typeof<obj> ; typeof<int> ] do
                    let e =
                        Assert.Throws<TargetInvocationException> (fun () ->
                            binds
                                .GetMethod(name)
                                .MakeGenericMethod(instantiation)
                                .Invoke ((null : obj), Array.empty<obj>)
                            |> ignore<obj>
                        )

                    e.InnerException.GetType () |> shouldEqual missing
        finally
            context.Unload ()

        let assembly =
            Assembly.read loggerFactory (Some "Binds.dll") (new MemoryStream (image))

        let analysis = analysisOver [ assembly ] id

        for name in [ "ReadField" ; "CallMethod" ] do
            let _, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly "W.Binds" name)

            escapes.Unknown |> shouldEqual true

    /// Where the object a `throw` raises comes from, in an emitted method.
    [<RequireQualifiedAccess>]
    type private Raise =
        /// `newobj object::.ctor; throw`.
        | Constructed
        /// `call object Make(); throw`, whose static type is only `object`.
        | Returned
        /// `call object MakeException(); throw`: an exception, whose static type is only `object`.
        | ReturnedException
        /// `call Throw()`, which throws a constructed object.
        | Callee
        /// `ldstr; newobj RuntimeWrappedException::.ctor(object); throw`: a wrapper thrown
        /// explicitly, which a clause in an assembly that does not wrap sees unwrapped.
        | ConstructedWrapper
        /// `call Exception MakeWrapper(); throw`: the same wrapper, whose static type is only
        /// `Exception`.
        | ReturnedWrapper
        /// `ldnull; throw`, whose operand's type the analysis does not know.
        | Untyped

    /// The `catch` clause, if any, around an emitted method's raise.
    [<RequireQualifiedAccess>]
    type private Clause =
        | Uncaught
        | Catch of ns : string * name : string

    let private raises : Raise list =
        [
            Raise.Constructed
            Raise.Returned
            Raise.ReturnedException
            Raise.Callee
            Raise.ConstructedWrapper
            Raise.ReturnedWrapper
            Raise.Untyped
        ]

    let private clauses : Clause list =
        [
            Clause.Uncaught
            Clause.Catch ("System", "Exception")
            Clause.Catch ("System.Runtime.CompilerServices", "RuntimeWrappedException")
            Clause.Catch ("System", "Object")
        ]

    let private emittedMethodName (raise : Raise) (clause : Clause) : string =
        match clause with
        | Clause.Uncaught -> $"%A{raise}"
        | Clause.Catch (_, name) -> $"%A{raise}_%s{name}"

    /// An assembly `W.Throws` with one static method per raise and clause, plus the helpers `Make`,
    /// `MakeException`, `Throw` and `MakeWrapper`, carrying one `RuntimeCompatibilityAttribute` per blob in `attributes`, in order.
    let private emitThrows (attributes : byte[] list) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Throws.dll",
            metadata.GetOrAddGuid (Guid "5a1e7c3b-9d2f-4b8e-a6c4-1f3e5d7b9a2c"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        let assemblyDefinition =
            metadata.AddAssembly (
                metadata.GetOrAddString "Throws",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                AssemblyHashAlgorithm.None
            )

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = typeRef "System" "Object"

        let signature (isInstance : bool) (returnsObject : bool) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = isInstance)
                .Parameters (
                    0,
                    (fun returnType ->
                        if returnsObject then
                            returnType.Type().Object ()
                        else
                            returnType.Void ()
                    ),
                    ignore<ParametersEncoder>
                )

            metadata.GetOrAddBlob blob

        let constructorOf (ty : EntityHandle) : EntityHandle =
            metadata.AddMemberReference (ty, metadata.GetOrAddString ".ctor", signature true false)
            |> MemberReferenceHandle.op_Implicit

        let objectConstructor = constructorOf objectRef
        let exceptionRef = typeRef "System" "Exception"
        let exceptionConstructor = constructorOf exceptionRef

        let wrapperConstructor : EntityHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters -> parameters.AddParameter().Type().Object ())
                )

            metadata.AddMemberReference (
                typeRef "System.Runtime.CompilerServices" "RuntimeWrappedException",
                metadata.GetOrAddString ".ctor",
                metadata.GetOrAddBlob blob
            )
            |> MemberReferenceHandle.op_Implicit

        let compatibilityConstructor =
            constructorOf (typeRef "System.Runtime.CompilerServices" "RuntimeCompatibilityAttribute")

        for blob in attributes do
            metadata.AddCustomAttribute (
                (AssemblyDefinitionHandle.op_Implicit assemblyDefinition : EntityHandle),
                compatibilityConstructor,
                metadata.GetOrAddBlob blob
            )
            |> ignore<CustomAttributeHandle>

        // Method definitions are numbered in the order they are added: `Make`, `MakeException`,
        // `Throw`, `MakeWrapper`, then the cases in the order of `raises` and `clauses`.
        let make = MetadataTokens.MethodDefinitionHandle 1
        let makeException = MetadataTokens.MethodDefinitionHandle 2
        let throw = MetadataTokens.MethodDefinitionHandle 3
        let makeWrapper = MetadataTokens.MethodDefinitionHandle 4

        let constructWrapper (code : InstructionEncoder) : unit =
            code.LoadString (metadata.GetOrAddUserString "sentinel")
            code.OpCode ILOpCode.Newobj
            code.Token wrapperConstructor

        let emitRaise (code : InstructionEncoder) (raise : Raise) : unit =
            match raise with
            | Raise.Constructed ->
                code.OpCode ILOpCode.Newobj
                code.Token objectConstructor
                code.OpCode ILOpCode.Throw
            | Raise.Returned ->
                code.Call make
                code.OpCode ILOpCode.Throw
            | Raise.ReturnedException ->
                code.Call makeException
                code.OpCode ILOpCode.Throw
            | Raise.Callee -> code.Call throw
            | Raise.ConstructedWrapper ->
                constructWrapper code
                code.OpCode ILOpCode.Throw
            | Raise.ReturnedWrapper ->
                code.Call makeWrapper
                code.OpCode ILOpCode.Throw
            | Raise.Untyped ->
                code.OpCode ILOpCode.Ldnull
                code.OpCode ILOpCode.Throw

        let addMethod (name : string) (returnsObject : bool) (body : int) : unit =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature false returnsObject,
                body,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        let makeBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Newobj
            code.Token objectConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        addMethod "Make" true makeBody

        let makeExceptionBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Newobj
            code.Token exceptionConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        addMethod "MakeException" true makeExceptionBody

        let throwBody =
            let code = InstructionEncoder (BlobBuilder ())
            emitRaise code Raise.Constructed
            bodies.AddMethodBody code

        addMethod "Throw" false throwBody

        let makeWrapperBody =
            let code = InstructionEncoder (BlobBuilder ())
            constructWrapper code
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let returnsException =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (
                    0,
                    (fun returnType -> returnType.Type().Type (exceptionRef, false)),
                    ignore<ParametersEncoder>
                )

            metadata.GetOrAddBlob blob

        metadata.AddMethodDefinition (
            MethodAttributes.Public ||| MethodAttributes.Static,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "MakeWrapper",
            returnsException,
            makeWrapperBody,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        for raise in raises do
            for clause in clauses do
                let body =
                    match clause with
                    | Clause.Uncaught ->
                        let code = InstructionEncoder (BlobBuilder ())
                        emitRaise code raise
                        code.OpCode ILOpCode.Ret
                        bodies.AddMethodBody code
                    | Clause.Catch (ns, name) ->
                        let flow = ControlFlowBuilder ()
                        let code = InstructionEncoder (BlobBuilder (), flow)
                        let tryStart = code.DefineLabel ()
                        let handlerStart = code.DefineLabel ()
                        let handlerEnd = code.DefineLabel ()
                        code.MarkLabel tryStart
                        emitRaise code raise
                        code.Branch (ILOpCode.Leave_s, handlerEnd)
                        code.MarkLabel handlerStart
                        code.OpCode ILOpCode.Pop
                        code.Branch (ILOpCode.Leave_s, handlerEnd)
                        code.MarkLabel handlerEnd
                        code.OpCode ILOpCode.Ret
                        flow.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, typeRef ns name)
                        bodies.AddMethodBody code

                addMethod (emittedMethodName raise clause) false body

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            make
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Throws",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            make
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray ()

    /// Run each of `methods` of `W.Throws` in `image` on the real runtime, answering which let an
    /// exception escape.
    let private escapingOnRealRuntime (image : byte[]) (methods : string list) : Map<string, bool> =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Throws", isCollectible = true)

        try
            let throws = context.LoadFromStream(new MemoryStream (image)).GetType "W.Throws"

            methods
            |> List.map (fun name ->
                let escaped =
                    try
                        throws.GetMethod(name).Invoke ((null : obj), Array.empty<obj>) |> ignore<obj>
                        false
                    with :? TargetInvocationException ->
                        true

                name, escaped
            )
            |> Map.ofList
        finally
            context.Unload ()

    [<Test>]
    let ``a thrown object that is not an exception is caught as the catching assembly sees it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let cases =
            [
                for raise in raises do
                    for clause in clauses do
                        raise, clause, emittedMethodName raise clause
            ]

        for wraps in [ false ; true ] do
            let image =
                emitThrows (
                    if wraps then
                        [
                            TestRuntimeCompatibility.compatibilityBlob
                                1s
                                [ TestRuntimeCompatibility.wrapProperty [ 1uy ] ]
                        ]
                    else
                        []
                )

            let assembly =
                Assembly.read loggerFactory (Some "Throws.dll") (new MemoryStream (image))

            RuntimeCompatibility.wrapsNonExceptionThrows assembly |> shouldEqual wraps

            // `ldnull; throw` raises a `NullReferenceException` at run time, which says nothing
            // about how an unknown exception is caught, so only the analysis is asked about those.
            let runtime =
                cases
                |> List.filter (fun (raise, _, _) -> raise <> Raise.Untyped)
                |> List.map (fun (_, _, name) -> name)
                |> escapingOnRealRuntime image

            let mutable analysis = analysisOver [ assembly ] id

            for raise, clause, name in cases do
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed assembly "W.Throws" name)

                analysis <- next

                let describe () =
                    let wrapping = if wraps then "wraps" else "does not wrap"
                    $"%s{name} in an assembly that %s{wrapping}: %A{render analysis escapes}, unknown %b{escapes.Unknown}"

                match raise with
                | Raise.Untyped ->
                    let absorbed =
                        match clause with
                        | Clause.Catch ("System", "Object") -> true
                        | Clause.Catch ("System", "Exception") -> wraps
                        | _ -> false

                    if escapes.Unknown = absorbed then
                        failwith $"%s{describe ()}; expected unknown %b{not absorbed}"
                | _ ->
                    // What the analysis reports the raise as.
                    let reported =
                        match raise with
                        | Raise.ConstructedWrapper -> [ "=System.Runtime.CompilerServices.RuntimeWrappedException" ]
                        | Raise.ReturnedWrapper -> [ "<:System.Exception" ]
                        | _ -> [ "=System.Object" ; "<:System.Object" ]

                    let analysisEscapes =
                        render analysis escapes
                        |> Set.exists (fun shown -> List.contains shown reported)

                    // Everything that escapes is reported. A returned object's type is known only
                    // as its static type, which may cover values a clause stops and values it does
                    // not, and whatever could not stop them all is reported to let it escape;
                    // otherwise the answer is exact.
                    let exact =
                        match raise with
                        | Raise.Returned
                        | Raise.ReturnedException
                        | Raise.ReturnedWrapper -> false
                        | _ -> true

                    if
                        (runtime.[name] && not analysisEscapes)
                        || (exact && analysisEscapes <> runtime.[name])
                    then
                        failwith $"%s{describe ()}; the runtime lets it escape: %b{runtime.[name]}"
