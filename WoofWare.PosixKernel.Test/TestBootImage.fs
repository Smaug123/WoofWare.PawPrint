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

    [<Test>]
    let ``every public setter takes and returns a boot image`` () : unit =
        let setters =
            publicFunctions
            // `withX`, the setter naming convention: `withoutTaking` is not one.
            |> List.filter (fun m ->
                m.Name.StartsWith ("with", StringComparison.Ordinal)
                && m.Name.Length > 4
                && Char.IsUpper m.Name.[4]
            )

        // The ones there are, so that this cannot pass by finding none.
        setters
        |> List.map (fun m -> m.Name)
        |> List.distinct
        |> List.sort
        |> String.concat " "
        |> shouldEqual (
            String.concat " "
            <| List.sort
                [
                    "withBootTime"
                    "withCoreDumps"
                    "withCredentials"
                    "withEnvironment"
                    "withEphemeralPortRange"
                    "withFileSystemAndCurrentDirectory"
                    "withLeaderThreadId"
                    "withLocalAddresses"
                    "withMount"
                    "withPipeDevice"
                    "withProcessId"
                    "withProcessPath"
                    "withProcessorCount"
                    "withProtectedFiles"
                    "withSoMaxConn"
                    "withTcpSendSpace"
                    "withUmask"
                    "withUserAddressLimit"
                ]
        )

        for setter in setters do
            let parameters = setter.GetParameters ()
            let last = parameters.[parameters.Length - 1].ParameterType

            if not (isGenericOf imageType last) then
                failwith $"%s{describe setter} takes a %s{last.Name} last, not a boot image."

            let returned = setter.ReturnType

            let returnsImage =
                isGenericOf imageType returned
                || (isGenericOf typedefof<Result<int, int>> returned
                    && isGenericOf imageType (returned.GetGenericArguments ()).[0])

            if not returnsImage then
                failwith $"%s{describe setter} returns a %s{returned.Name}, not a boot image."

    [<Test>]
    let ``only boot makes a system from an image, and nothing makes an image from a system`` () : unit =
        let takes (definition : Type) (m : MethodInfo) : bool =
            m.GetParameters ()
            |> Array.exists (fun p -> isGenericOf definition p.ParameterType)

        publicFunctions
        |> List.filter (fun m -> takes imageType m && isGenericOf systemType m.ReturnType)
        |> List.map describe
        |> shouldEqual [ "WoofWare.PosixKernel.UnixBootImage.boot" ]

        publicFunctions
        |> List.filter (fun m -> takes systemType m && isGenericOf imageType m.ReturnType)
        |> List.map describe
        |> shouldEqual []

    [<Test>]
    let ``a boot image is opaque`` () : unit =
        let t = typeof<UnixBootImage<int, string>>

        FSharpType.IsRecord (t, BindingFlags.Public) |> shouldEqual false
        t.GetProperties (BindingFlags.Public ||| BindingFlags.Instance) |> shouldBeEmpty

        t.GetConstructors (BindingFlags.Public ||| BindingFlags.Instance)
        |> shouldBeEmpty
