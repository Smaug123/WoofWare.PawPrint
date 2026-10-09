namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What the library lets a client do to its tables. The filesystem, the
/// descriptor tables, the open file descriptions and the machine change only
/// inside a syscall, so a client reads them out of a system and never gets a
/// changed one back from the library.
[<TestFixture>]
module TestPublicSurface =

    /// The kernel's tables. Each has a representation hidden from clients, so
    /// a client holds one only by reading it out of a system.
    let private tables : Type list =
        [
            typeof<VirtualFileSystem>
            typeof<FileDescriptorRegistry>
            typeof<DescriptorTable>
            typeof<OpenFileTable>
            typeof<UnixMachineState>
        ]

    /// Every type `t` is built from, `t` included: generic arguments (which is
    /// where a tuple, a `Result`, an option or a function keeps its parts) and
    /// array elements.
    let rec private constituents (t : Type) : Type list =
        let parts =
            if t.HasElementType then
                [ t.GetElementType () ]
            elif t.IsGenericType then
                List.ofArray (t.GetGenericArguments ())
            else
                []

        t :: List.collect constituents parts

    let private mentions (table : Type) (t : Type) : bool =
        constituents t
        |> List.exists (fun (c : Type) -> c = table || (c.IsGenericType && c.GetGenericTypeDefinition () = table))

    /// Every public method the library exports that takes one of the tables
    /// and returns one of the same type: a step that changes that table
    /// outside a syscall.
    let private tableRewriters (assembly : Assembly) : string list =
        assembly.GetExportedTypes ()
        |> Array.toList
        |> List.collect (fun (t : Type) ->
            t.GetMethods (
                BindingFlags.Public
                ||| BindingFlags.Static
                ||| BindingFlags.Instance
                ||| BindingFlags.DeclaredOnly
            )
            |> Array.toList
            |> List.filter (fun (m : MethodInfo) ->
                let inputs =
                    [
                        if not m.IsStatic then
                            yield t
                        for p in m.GetParameters () do
                            yield p.ParameterType
                    ]

                tables
                |> List.exists (fun (table : Type) ->
                    mentions table m.ReturnType && List.exists (mentions table) inputs
                )
            )
            |> List.map (fun (m : MethodInfo) -> $"%s{t.FullName}.%s{m.Name}")
        )

    [<Test>]
    let ``no public function takes one of the kernel's tables and returns a changed one`` () : unit =
        let assembly = typeof<VirtualFileSystem>.Assembly

        // A control, so that this cannot pass by finding no functions at all:
        // the queries that read a table out of a system are public.
        let reads = assembly.GetType ("WoofWare.PosixKernel.UnixSystem", true)

        [ "fileSystem" ; "fileDescriptors" ; "openFiles" ]
        |> List.filter (fun (name : string) ->
            isNull (reads.GetMethod (name, BindingFlags.Public ||| BindingFlags.Static))
        )
        |> shouldBeEmpty

        tableRewriters assembly |> shouldBeEmpty
