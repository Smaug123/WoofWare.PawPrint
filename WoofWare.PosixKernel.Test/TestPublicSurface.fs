namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What the library lets a client call. The filesystem, the descriptor
/// tables, the open file descriptions and the machine change only inside a
/// syscall, so a client reads them out of a system and never gets a changed
/// one back from the library. And a client holds only what the library hands
/// it, so no public function takes a type that nothing public hands out.
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

    /// What a public member asks of its caller and hands back, each as the
    /// library's own types it mentions (by generic definition): a client must
    /// already hold a value of every type in `Needs`, and is handed one of
    /// every type in `Gives`.
    type private Signature =
        {
            Name : string
            /// Whether this is a function of an F# module, as opposed to a
            /// constructor, a property or a union case's maker.
            IsModuleFunction : bool
            Needs : Type list
            Gives : Type list
        }

    let private definitionOf (t : Type) : Type =
        if t.IsGenericType && not t.IsGenericTypeDefinition then
            t.GetGenericTypeDefinition ()
        else
            t

    /// `Type` has no ordering, so a set of types is kept by full name.
    let private keyOf (t : Type) : string = t.FullName

    let private isModule (t : Type) : bool =
        t.GetCustomAttributes<CompilationMappingAttribute> ()
        |> Seq.exists (fun (a : CompilationMappingAttribute) ->
            a.SourceConstructFlags &&& SourceConstructFlags.KindMask = SourceConstructFlags.Module
        )

    /// The library's types a value of type `t` holds, which a client holding
    /// one can take out of it.
    let rec private held (library : Assembly) (t : Type) : Type list =
        if t.IsGenericParameter then
            []
        elif t.HasElementType then
            held library (t.GetElementType ())
        else
            let own = if t.Assembly = library then [ definitionOf t ] else []

            let parts =
                if t.IsGenericType then
                    t.GetGenericArguments () |> List.ofArray |> List.collect (held library)
                else
                    []

            own @ parts

    /// What a client must hold to pass a value of type `t`, and what it is
    /// handed back when `t` is a function the library calls: that function's
    /// arguments.
    let rec private passing (library : Assembly) (t : Type) : Type list * Type list =
        if t.IsGenericParameter then
            [], []
        elif t.HasElementType then
            passing library (t.GetElementType ())
        elif t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<int -> int> then
            let arguments = t.GetGenericArguments ()
            let needs, gives = passing library arguments.[1]
            needs, held library arguments.[0] @ gives
        else
            let own = if t.Assembly = library then [ definitionOf t ] else []

            let parts =
                if t.IsGenericType then
                    t.GetGenericArguments () |> List.ofArray |> List.map (passing library)
                else
                    []

            own @ List.collect fst parts, List.collect snd parts

    let private signatures (library : Assembly) : Signature list =
        let exported = library.GetExportedTypes () |> List.ofArray

        [
            for t in exported do
                let inModule = isModule t

                for m in
                    t.GetMethods (
                        BindingFlags.Public
                        ||| BindingFlags.Static
                        ||| BindingFlags.Instance
                        ||| BindingFlags.DeclaredOnly
                    ) do
                    let parameters =
                        m.GetParameters ()
                        |> List.ofArray
                        |> List.map (fun (p : ParameterInfo) -> passing library p.ParameterType)

                    yield
                        {
                            Name = $"%s{t.FullName}.%s{m.Name}"
                            // A name with an `@` in it is the compiler's, not the
                            // library's: a debugging copy of an inline function, say.
                            IsModuleFunction = inModule && m.IsStatic && not (m.Name.Contains '@')
                            Needs = (if m.IsStatic then [] else [ definitionOf t ]) @ List.collect fst parameters
                            Gives = held library m.ReturnType @ List.collect snd parameters
                        }

                for c in t.GetConstructors (BindingFlags.Public ||| BindingFlags.Instance) do
                    let parameters =
                        c.GetParameters ()
                        |> List.ofArray
                        |> List.map (fun (p : ParameterInfo) -> passing library p.ParameterType)

                    yield
                        {
                            Name = $"%s{t.FullName}..ctor"
                            IsModuleFunction = false
                            Needs = List.collect fst parameters
                            Gives = definitionOf t :: List.collect snd parameters
                        }

                if t.IsEnum then
                    yield
                        {
                            Name = $"%s{t.FullName} (enum)"
                            IsModuleFunction = false
                            Needs = []
                            Gives = [ t ]
                        }
        ]

    /// Every type of the library a client can come to hold, starting from
    /// nothing and calling only public members with what it already holds. A
    /// union case's own class is held once its union is, since matching on the
    /// union hands it over.
    let private reachable (library : Assembly) (signatures : Signature list) : Set<string> =
        let nested =
            library.GetExportedTypes ()
            |> List.ofArray
            |> List.filter (fun (t : Type) -> t.IsNested)

        let rec grow (held : Set<string>) : Set<string> =
            let fromCalls =
                signatures
                |> List.filter (fun (s : Signature) -> s.Needs |> List.forall (fun t -> held.Contains (keyOf t)))
                |> List.collect (fun (s : Signature) -> s.Gives |> List.map keyOf)
                |> Set.ofList

            let fromMatches =
                nested
                |> List.filter (fun (t : Type) -> held.Contains (keyOf (definitionOf t.DeclaringType)))
                |> List.map keyOf
                |> Set.ofList

            let next = Set.unionMany [ held ; fromCalls ; fromMatches ]
            if next = held then held else grow next

        grow Set.empty

    [<Test>]
    let ``every public function takes only what a client can come to hold`` () : unit =
        let library = typeof<VirtualFileSystem>.Assembly
        let signatures = signatures library
        let held = reachable library signatures

        let functions =
            signatures |> List.filter (fun (s : Signature) -> s.IsModuleFunction)

        // Controls, so that this cannot pass by finding no functions, or by
        // finding every type held: a client reaches a system and its
        // descriptor table, and never a bare descriptor table.
        functions.Length |> shouldBeGreaterThan 500

        held.Contains (keyOf typedefof<UnixSystem<int, string>>) |> shouldEqual true
        held.Contains (keyOf typeof<FileDescriptorRegistry>) |> shouldEqual true
        held.Contains (keyOf typeof<DescriptorTable>) |> shouldEqual false

        functions
        |> List.choose (fun (s : Signature) ->
            match
                s.Needs
                |> List.filter (fun (t : Type) -> not (held.Contains (keyOf t)))
                |> List.distinct
            with
            | [] -> None
            | missing ->
                let names = missing |> List.map (fun (t : Type) -> t.Name) |> String.concat ", "
                Some $"%s{s.Name} takes %s{names}"
        )
        |> shouldBeEmpty
