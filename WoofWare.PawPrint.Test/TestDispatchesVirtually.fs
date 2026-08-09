namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodInfo.DispatchesVirtually` decides whether a `callvirt` or `ldvirtftn` naming a method
/// resolves against the receiver's runtime type or binds to the named declaration outright. It is
/// the one place the two opcodes have to agree, and getting it backwards is not something a guest
/// program can catch: Roslyn never emits `ldvirtftn` naming a `static`, `final` or non-`virtual`
/// method — even through a `sealed` receiver, and even for a `sealed override`, the token names the
/// least-derived non-final declaration — so no differential test can reach the non-dispatching
/// branch at all.
///
/// The oracle is therefore outside PawPrint entirely: load a real assembly with `System.Reflection`
/// and ask the CLR what it thinks of every single method, then check PawPrint's own reading of the
/// same metadata agrees. FSharp.Core is the corpus because this test project already runs against
/// it (so there is no environmental precondition) and because it is large and varied enough to
/// contain every flag combination — which the fixture asserts rather than assumes, so a corpus that
/// stopped exercising a case would fail here rather than quietly stop testing it.
[<TestFixture>]
module TestDispatchesVirtually =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corpusAssembly : Assembly = typeof<int list>.Assembly

    let private dumped : DumpedAssembly =
        Assembly.readFile loggerFactory corpusAssembly.Location

    /// How a method fails to dispatch virtually, or that it does. Counted below so the fixture can
    /// prove it actually exercised each case.
    [<RequireQualifiedAccess>]
    type private Shape =
        | Dispatches
        | Static
        | NonVirtual
        | FinalVirtual

    let private shapeOf (m : MethodBase) : Shape =
        if m.IsStatic then Shape.Static
        elif not m.IsVirtual then Shape.NonVirtual
        elif m.IsFinal then Shape.FinalVirtual
        else Shape.Dispatches

    [<Test>]
    let ``DispatchesVirtually agrees with the CLR's own view of every method in FSharp.Core`` () =
        let reflectionModule = corpusAssembly.Modules |> Seq.exactlyOne
        let counts = Dictionary<Shape, int> ()
        let mutable checked' = 0

        for KeyValue (handle, method) in dumped.Methods do
            let token =
                MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit handle : EntityHandle)

            let reflected = reflectionModule.ResolveMethod token

            let shape = shapeOf reflected

            counts.[shape] <-
                (match counts.TryGetValue shape with
                 | true, n -> n
                 | false, _ -> 0)
                + 1

            checked' <- checked' + 1

            if method.DispatchesVirtually <> (shape = Shape.Dispatches) then
                failwith
                    $"%s{reflected.DeclaringType.FullName}::%s{reflected.Name} (token 0x%08x{token}): PawPrint says DispatchesVirtually = %b{method.DispatchesVirtually}, but the CLR reports IsStatic = %b{reflected.IsStatic}, IsVirtual = %b{reflected.IsVirtual}, IsFinal = %b{reflected.IsFinal}"

        // Non-vacuity: every way of answering the question must actually have occurred.
        for shape in [ Shape.Dispatches ; Shape.Static ; Shape.NonVirtual ; Shape.FinalVirtual ] do
            match counts.TryGetValue shape with
            | true, n when n > 0 -> ()
            | _ -> failwith $"corpus contained no method of shape %O{shape}; this fixture is not testing what it claims"

        // A corpus this size is what makes the agreement meaningful; a truncated read would pass
        // vacuously otherwise.
        checked' |> shouldBeGreaterThan 5000
