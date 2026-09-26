namespace WoofWare.PawPrint.Test

open System
open System.IO
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
            // `Boom`'s initializer fails, and calling `M` runs it first.
            { expect "Fixture.CctorCases" "CallsBoom" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            { expect "Fixture.ShadowCases" "DereferencesNull" with
                Contains = [ "=System.NullReferenceException" ]
            }
        ]

    [<Test>]
    let ``each fixture method's escaping exceptions are as stated`` () : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; fixture ]

        let mutable analysis =
            EscapeAnalysis.create
                loggerFactory
                runtimeDirs
                {
                    ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                    LoadedAssemblies = loaded
                    BaseTypes = baseClassTypes
                }

        let methodNamed (typeName : string, methodName : string) : MethodKey =
            fixture.Methods
            |> Seq.pick (fun (KeyValue (handle, method)) ->
                if
                    method.Name = methodName
                    && TypeInfo.fullName
                        (fun h -> fixture.TypeDefs.[h])
                        fixture.TypeDefs.[method.RequiredDeclaringType.Definition.Get] = typeName
                then
                    Some (MethodKey.make fixture handle)
                else
                    None
            )

        let failures = ResizeArray<string> ()

        for expectation in expectations do
            let next, escapes = EscapeAnalysis.escapes analysis (methodNamed expectation.Method)
            analysis <- next

            let shown =
                escapes.Types
                |> Seq.map (fun thrown ->
                    match thrown with
                    | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
                    | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
                )
                |> Set.ofSeq

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
