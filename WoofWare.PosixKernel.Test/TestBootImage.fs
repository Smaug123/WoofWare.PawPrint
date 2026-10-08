namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// That configuration cannot be applied to a system that has already run.
///
/// That is a compile-time property: a setter takes a `UnixBootImage`, a syscall
/// takes a `UnixSystem`, and the only way from one to the other is
/// `UnixBootImage.boot`, so applying a setter after boot does not type-check.
/// No runtime test can observe a program that does not compile. What these
/// tests check is the library's public signatures that make the compiler refuse
/// it, so that a setter added later on a booted system, or a way back from a
/// system to an image, fails here.
[<TestFixture>]
module TestBootImage =

    let private library : Assembly = typeof<UnixBootImage<int, string>>.Assembly

    let private isGenericOf (definition : Type) (candidate : Type) : bool =
        candidate.IsGenericType && candidate.GetGenericTypeDefinition () = definition

    let private imageType : Type = typedefof<UnixBootImage<int, string>>
    let private systemType : Type = typedefof<UnixSystem<int, string>>

    /// Every public static method of every public type in the library: an F#
    /// module's functions are among them.
    let private publicFunctions : MethodInfo list =
        library.GetExportedTypes ()
        |> Seq.collect (fun t ->
            t.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
        )
        |> List.ofSeq

    let private describe (m : MethodInfo) : string =
        $"%s{m.DeclaringType.FullName}.%s{m.Name}"

    let private launchType : Type = typedefof<ProcessLaunch<int>>

    /// `withX`, the setter naming convention: `withoutTaking` is not one.
    let private setters : MethodInfo list =
        publicFunctions
        |> List.filter (fun m ->
            m.Name.StartsWith ("with", StringComparison.Ordinal)
            && m.Name.Length > 4
            && Char.IsUpper m.Name.[4]
        )

    /// That `setter` takes a `configured` last and returns one, or a `Result`
    /// of one.
    let private configures (configured : Type) (setter : MethodInfo) : unit =
        let parameters = setter.GetParameters ()
        let last = parameters.[parameters.Length - 1].ParameterType

        if not (isGenericOf configured last) then
            failwith $"%s{describe setter} takes a %s{last.Name} last, not a %s{configured.Name}."

        let returned = setter.ReturnType

        let returnsConfigured =
            isGenericOf configured returned
            || (isGenericOf typedefof<Result<int, int>> returned
                && isGenericOf configured (returned.GetGenericArguments ()).[0])

        if not returnsConfigured then
            failwith $"%s{describe setter} returns a %s{returned.Name}, not a %s{configured.Name}."

    [<Test>]
    let ``every public setter configures a boot image or a process launch`` () : unit =
        let declaredIn (moduleName : string) : MethodInfo list =
            setters
            |> List.filter (fun m -> m.DeclaringType.FullName = $"WoofWare.PosixKernel.%s{moduleName}")

        // The ones there are, so that this cannot pass by finding none.
        let names (methods : MethodInfo list) : string =
            methods
            |> List.map (fun m -> m.Name)
            |> List.distinct
            |> List.sort
            |> String.concat " "

        (declaredIn "UnixBootImage" @ declaredIn "ProcessLaunch")
        |> List.length
        |> shouldEqual setters.Length

        declaredIn "UnixBootImage"
        |> names
        |> shouldEqual (
            String.concat " "
            <| List.sort
                [
                    "withBootTime"
                    "withEntropySeed"
                    "withEphemeralPortRange"
                    "withFileSystem"
                    "withIpv6OnlyByDefault"
                    "withLeaderThreadId"
                    "withLocalAddresses"
                    "withMount"
                    "withPipeDevice"
                    "withProcessId"
                    "withProcessorCount"
                    "withProtectedFiles"
                    "withSoMaxConn"
                    "withTcpReceiveSpace"
                    "withTcpSendSpace"
                    "withTcpSendSpaceMax"
                    "withUserAddressLimit"
                ]
        )

        declaredIn "ProcessLaunch"
        |> names
        |> shouldEqual (
            String.concat " "
            <| List.sort
                [
                    "withCoreDumps"
                    "withCredentials"
                    "withCurrentDirectory"
                    "withEnvironment"
                    "withProcessPath"
                    "withUmask"
                ]
        )

        for setter in declaredIn "UnixBootImage" do
            configures imageType setter

        for setter in declaredIn "ProcessLaunch" do
            configures launchType setter

    [<Test>]
    let ``only boot makes a system from an image, and nothing makes an image from a system`` () : unit =
        let takes (definition : Type) (m : MethodInfo) : bool =
            m.GetParameters ()
            |> Array.exists (fun p -> isGenericOf definition p.ParameterType)

        let makesSystem (m : MethodInfo) : bool =
            isGenericOf systemType m.ReturnType
            || (isGenericOf typedefof<Result<int, int>> m.ReturnType
                && isGenericOf systemType (m.ReturnType.GetGenericArguments ()).[0])

        publicFunctions
        |> List.filter (fun m -> takes imageType m && makesSystem m)
        |> List.map describe
        |> shouldEqual [ "WoofWare.PosixKernel.UnixBootImage.boot" ]

        publicFunctions
        |> List.filter (fun m -> takes systemType m && isGenericOf imageType m.ReturnType)
        |> List.map describe
        |> shouldEqual []

    [<Test>]
    let ``a process starts only from a launch, by boot or by SimulatedMachine.launch`` () : unit =
        let takes (definition : Type) (m : MethodInfo) : bool =
            m.GetParameters ()
            |> Array.exists (fun p -> isGenericOf definition p.ParameterType)

        publicFunctions
        |> List.filter (fun m -> takes launchType m && not (isGenericOf launchType m.ReturnType))
        |> List.filter (fun m ->
            not (
                isGenericOf typedefof<Result<int, int>> m.ReturnType
                && isGenericOf launchType (m.ReturnType.GetGenericArguments ()).[0]
            )
        )
        |> List.map describe
        |> List.sort
        |> shouldEqual
            [
                "WoofWare.PosixKernel.ProcessLaunch.leader"
                "WoofWare.PosixKernel.ProcessLaunch.platform"
                "WoofWare.PosixKernel.SimulatedMachine.launch"
                "WoofWare.PosixKernel.UnixBootImage.boot"
            ]

    [<Test>]
    let ``a boot image is opaque`` () : unit =
        let t = typeof<UnixBootImage<int, string>>

        FSharpType.IsRecord (t, BindingFlags.Public) |> shouldEqual false
        t.GetProperties (BindingFlags.Public ||| BindingFlags.Instance) |> shouldBeEmpty

        t.GetConstructors (BindingFlags.Public ||| BindingFlags.Instance)
        |> shouldBeEmpty

    /// A client that could copy a booted system's records could set anything
    /// a setter sets, or anything a syscall changes, without going through
    /// either. So the records a system is made of have no public fields and no
    /// public constructor: a client reads them through queries and changes them
    /// through syscalls.
    [<Test>]
    let ``a booted system's state is opaque`` () : unit =
        let records : Type list =
            [
                typeof<UnixSystem<int, string>>
                typeof<SimulatedMachine<int, string>>
                typeof<ProcessSlot<int, string>>
                typeof<ProcessLaunch<int>>
                typeof<UnixMachineState>
                typeof<UnixProcessState<int, string>>
                typeof<UnixTaskState>
            ]

        for t in records do
            // That they are records at all, so that the rest cannot pass by
            // asking about the wrong types.
            FSharpType.IsRecord (t, BindingFlags.NonPublic) |> shouldEqual true
            FSharpType.IsRecord (t, BindingFlags.Public) |> shouldEqual false

            t.GetProperties (BindingFlags.Public ||| BindingFlags.Instance)
            |> Array.map (fun p -> p.Name)
            |> shouldBeEmpty

            t.GetConstructors (BindingFlags.Public ||| BindingFlags.Instance)
            |> shouldBeEmpty

    /// Parking a task and releasing its park change a running system without
    /// being syscalls, so a client parks a task by making a blocking syscall and
    /// releases it by finishing one.
    [<Test>]
    let ``parking and releasing are not public`` () : unit =
        let named (declaringPrefix : string) (name : string) : MethodInfo list =
            library.GetTypes ()
            |> Seq.filter (fun t -> t.FullName.StartsWith (declaringPrefix, StringComparison.Ordinal))
            |> Seq.collect (fun t ->
                t.GetMethods (
                    BindingFlags.Public
                    ||| BindingFlags.NonPublic
                    ||| BindingFlags.Static
                    ||| BindingFlags.DeclaredOnly
                )
            )
            |> Seq.filter (fun m -> m.Name = name)
            |> List.ofSeq

        for declaring, name in
            [
                "WoofWare.PosixKernel.UnixWait", "park"
                "WoofWare.PosixKernel.UnixTaskTable", "unpark"
            ] do
            match named declaring name with
            | [ m ] -> (describe m, m.IsPublic) |> shouldEqual (describe m, false)
            | other -> failwith $"expected one %s{declaring}.%s{name}, found %d{List.length other}"
