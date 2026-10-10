namespace WoofWare.PosixKernel.Test

open System.Collections.Concurrent
open FsCheck
open FsCheck.FSharp

/// How many times a property reached each label over the fixed sample that
/// `CoverageSample.check` draws. Counting is thread-safe, so the property may
/// run its cases in parallel.
[<Sealed>]
type Coverage<'label when 'label : equality> internal () =
    let counts = ConcurrentDictionary<'label, int> ()

    member internal _.Hit (label : 'label) : unit =
        counts.AddOrUpdate (label, 1, (fun _ n -> n + 1)) |> ignore

    /// How many times the fixed sample reached `label`: zero for a label it
    /// never reached.
    member _.Count (label : 'label) : int =
        match counts.TryGetValue label with
        | true, n -> n
        | false, _ -> 0

    /// Every label the fixed sample reached, each with its count.
    member _.Reached : ('label * int) list =
        counts |> Seq.map (fun kv -> kv.Key, kv.Value) |> List.ofSeq

/// Checking a property whose test also asserts coverage: that its cases reach
/// each of some labels, or reach it often enough.
///
/// Such an assertion is a claim about the generator, and counted over fresh
/// cases it fails now and then by chance. So `check` counts over a fixed
/// sample, the same cases on every run, and then checks the property over as
/// many fresh cases, counting nothing. A floor asserted over the `Coverage` it
/// returns holds on every run or fails on every run, and a change to the
/// generator that loses a label fails on its own PR.
///
/// A label the fixed sample reaches only a few times can be lost by any
/// change that reshuffles the sample. Make it common rather than look for
/// another sample: for instance, begin some cases with a fixed opening that
/// reaches it.
[<RequireQualifiedAccess>]
module CoverageSample =

    /// The seed every fixed sample is replayed from. It is not a parameter, so
    /// that a lost label is fixed in the generator rather than by a seed that
    /// happens to reach it.
    let private seed : Rnd = Rnd 20261010UL

    /// `config`, running four cases at a time.
    let inParallel (config : Config) : Config =
        config.WithParallelRunConfig (
            Some
                {
                    MaxDegreeOfParallelism = 4
                }
        )

    /// Checks `property` over `config.MaxTest` cases replayed from a fixed
    /// seed, giving it a `cover` that counts into the `Coverage` this returns;
    /// then over `config.MaxTest` fresh cases, giving it a `cover` that counts
    /// nothing. `config` must not already replay a seed. If it runs cases in
    /// parallel, `property` must be safe to run so.
    let checkProperty<'label when 'label : equality>
        (config : Config)
        (property : ('label -> unit) -> Property)
        : Coverage<'label>
        =
        if config.Replay.IsSome then
            failwith
                "CoverageSample.checkProperty: the config already replays a seed, but the fixed sample's seed is CoverageSample's own"

        let coverage = Coverage<'label> ()

        Check.One (
            config.WithReplay (
                Some
                    {
                        Rnd = seed
                        Size = None
                    }
            ),
            property coverage.Hit
        )

        for label, count in coverage.Reached |> List.sortBy (fun (label, _) -> sprintf "%A" label) do
            System.Console.WriteLine $"fixed sample reached %A{label} %d{count} times"

        Check.One (config, property ignore)
        coverage

    /// `checkProperty` over the cases `arbitrary` generates.
    let check<'a, 'label, 'testable when 'label : equality>
        (config : Config)
        (arbitrary : Arbitrary<'a>)
        (property : ('label -> unit) -> 'a -> 'testable)
        : Coverage<'label>
        =
        checkProperty config (fun cover -> Prop.forAll arbitrary (property cover))
