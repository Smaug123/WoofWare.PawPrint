namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeAssembly =
    open System.Collections.Immutable

    /// <summary>
    /// How <c>AssemblyNative_GetTypeCore</c> answers when following a forwarder did not produce a
    /// type.
    /// </summary>
    /// <remarks>
    /// Three cases because .NET 10 does three different things, measured at both
    /// <c>throwOnError</c> settings. They are not interchangeable: collapsing any two makes one of
    /// the two answers wrong.
    /// </remarks>
    [<RequireQualifiedAccess>]
    type private ForwarderMiss =
        /// Leave <c>retType</c> null and let <c>TypeNameResolver</c> decide whether that becomes a
        /// <c>TypeLoadException</c>, which it does only when the caller asked to be thrown at.
        | AnswerNull

        /// Raise <c>FileNotFoundException</c>, which <c>RuntimeAssembly.GetTypeCore</c> catches
        /// itself when the caller did not ask to be thrown at.
        | AssemblyUnavailable of WoofWare.PawPrint.AssemblyReference

        /// Raise <c>TypeLoadException</c>. Nothing on the way out catches it, so the guest sees it
        /// whichever way it asked.
        | BaseTypeAbsent of TypeResolutionMiss

    let private splitAtLastDot (name : string) : string * string =
        // CoreCLR's ns::FindSep walks back from the end and splits at the
        // final '.': everything before becomes the namespace, everything
        // after becomes the simple name. Names with no '.' are top-level
        // (empty namespace).
        let idx = name.LastIndexOf '.'

        if idx < 0 then
            "", name
        else
            name.Substring (0, idx), name.Substring (idx + 1)

    /// <summary>
    /// The eight-byte strong name token of a public key: ECMA-335 II.6.3's low eight bytes of
    /// the key's SHA-1 hash, in reverse order.
    /// </summary>
    /// <remarks>
    /// CoreCLR's <c>StrongNameTokenFromPublicKey</c> consults a table of six well-known keys
    /// before hashing. That table is a pure cache and is deliberately not reproduced: every
    /// one of its declared tokens is exactly what this computes, checked key by key against
    /// the pinned sources — including the ECMA neutral key, whose
    /// <c>b77a5c561934e089</c> is its own SHA-1 despite the surrounding code calling that key
    /// one that "doesn't look like a valid key".
    ///
    /// Its <c>StrongNameIsValidPublicKey</c> precondition is not reproduced either, for a
    /// different reason: it rejects a malformed blob with <c>CORSEC_E_INVALID_PUBLICKEY</c>,
    /// which the caller throws, but no such blob can reach here. PawPrint registers an
    /// assembly under its display name, and computing that already derives a token and throws
    /// on a key it cannot parse — so an image with an unparseable key fails to load long
    /// before a guest can ask for its name. Pinned by the test <c>an assembly with a malformed
    /// public key never loads</c>, which also checks the <c>afPublicKey</c> bit being clear is
    /// not a way around it (CoreCLR force-sets that bit for any non-empty blob on a manifest
    /// row, so the blob is never treated as an already-computed token).
    /// </remarks>
    let publicKeyToken (publicKey : byte[]) : byte[] =
        let hash = System.Security.Cryptography.SHA1.HashData publicKey
        hash.[hash.Length - 8 ..] |> Array.rev

    /// <summary>
    /// Escapes one segment of a display name, as
    /// <c>TextualIdentityParser::EscapeString</c> does.
    /// </summary>
    /// <remarks>
    /// Not the same algorithm as the BCL's, which is why <c>AssemblyName.FullName</c> is not
    /// an answer for <c>GetFullName</c>. Both escape <c>=</c>, <c>,</c>, <c>\</c>, tab,
    /// newline and carriage return, and both quote a segment with leading or trailing
    /// whitespace. They part company on embedded quotes: this one quotes with whichever of
    /// <c>'</c> or <c>"</c> the segment does not itself contain and then leaves that quote
    /// unescaped, so <c>Fn"Probe</c> becomes <c>'Fn"Probe'</c> where the BCL produces
    /// <c>"Fn\"Probe"</c>.
    /// </remarks>
    let escapeDisplayNameSegment (segment : string) : string =
        let isWhitespace (c : char) : bool =
            c = '\n' || c = '\r' || c = ' ' || c = '\t'

        let built = System.Text.StringBuilder ()

        // Leading or trailing whitespace requires quoting. CoreCLR reads both ends before it
        // starts, so this decision is made on the input rather than as it goes — and an empty
        // segment never reaches here, because `ToString` returns early on an empty simple
        // name and substitutes "neutral" for an empty culture.
        let mutable needQuotes =
            segment.Length > 0
            && (isWhitespace segment.[0] || isWhitespace segment.[segment.Length - 1])

        let mutable quoteChar = '"'

        for c in segment do
            match c with
            | '"'
            | '\'' ->
                if needQuotes && quoteChar <> c then
                    // Already quoting with the *other* character, so this one is unambiguous.
                    built.Append c |> ignore<System.Text.StringBuilder>
                elif not needQuotes then
                    // First quote seen: start quoting with the opposite character so this one
                    // needs no escape. Note this can be reached after characters have already
                    // been appended, which is exactly why the quotes are added at the end.
                    needQuotes <- true
                    quoteChar <- (if c = '"' then '\'' else '"')
                    built.Append c |> ignore<System.Text.StringBuilder>
                else
                    built.Append('\\').Append c |> ignore<System.Text.StringBuilder>
            | '='
            | ','
            | '\\' -> built.Append('\\').Append c |> ignore<System.Text.StringBuilder>
            | '\t' -> built.Append "\\t" |> ignore<System.Text.StringBuilder>
            | '\n' -> built.Append "\\n" |> ignore<System.Text.StringBuilder>
            | '\r' -> built.Append "\\r" |> ignore<System.Text.StringBuilder>
            | c -> built.Append c |> ignore<System.Text.StringBuilder>

        if needQuotes then
            $"%c{quoteChar}%s{built.ToString ()}%c{quoteChar}"
        else
            built.ToString ()

    /// <summary>
    /// The <c>processorArchitecture=</c> value CoreCLR reports for a manifest <c>Flags</c>
    /// column, or <c>None</c> when it reports no such segment.
    /// </summary>
    /// <remarks>
    /// <c>afPA_Mask</c> is <c>0x70</c>, a three-bit *field* whose values run
    /// MSIL/x86/IA64/AMD64/ARM/ARM64/NoPlatform — but
    /// <c>GetProcessorArchitectureFromAssemblyFlags</c> bit-*tests* it in priority order
    /// (<c>flags &amp; afPA_MSIL</c>, then <c>flags &amp; afPA_x86</c>, …), so overlapping
    /// values fall through to whichever name is tested first. The mapping that results is
    /// therefore not the one the field names suggest, and is reproduced rather than corrected
    /// because it is what a guest observes:
    ///
    /// <code>
    /// field   0x10  0x20  0x30  0x40  0x50  0x60  0x70
    /// name    MSIL  x86   IA64  AMD64 ARM   ARM64 NoPlatform
    /// answer  MSIL  x86   MSIL  AMD64 MSIL  x86   MSIL
    /// </code>
    ///
    /// So <c>IA64</c> and <c>ARM</c>, though present in CoreCLR's own name table, name no
    /// reachable value. Confirmed on the real runtime for all seven, not read off alone.
    /// </remarks>
    let processorArchitectureSegment (flags : int) : string option =
        // Emitted at all only when some bit of the field is set — `(m_dwFlags & afPA_Mask)`
        // in `GetDisplayName`, tested before the mapping below is consulted.
        if flags &&& 0x70 = 0 then
            None
        elif flags &&& 0x10 <> 0 then
            Some "MSIL"
        elif flags &&& 0x20 <> 0 then
            Some "x86"
        // Transcribed in CoreCLR's order even though this arm cannot fire: reaching it means
        // both 0x10 and 0x20 are clear, so `flags &&& 0x30` is zero too.
        elif flags &&& 0x30 <> 0 then
            Some "IA64"
        elif flags &&& 0x40 <> 0 then
            Some "AMD64"
        else
            // `afPA_ARM64` (0x60) is tested last in CoreCLR and is unreachable, because 0x60
            // carries 0x20 and so has already answered x86. Every value of the field is
            // covered above; nothing is left for this arm.
            failwith
                $"processorArchitectureSegment: assembly flags 0x%08X{flags} set a processor-architecture bit that no CoreCLR arm claims, which should be impossible for a three-bit field"

    /// <summary>
    /// The display name CoreCLR reports for an assembly, i.e.
    /// <c>TextualIdentityParser::ToString</c> over the <c>ASM_DISPLAYF_FULL</c> projection of
    /// the manifest's single <c>Assembly</c> row.
    /// </summary>
    /// <remarks>
    /// Built from the raw columns rather than from <c>DumpedAssembly.DefinitionFullName</c>, which
    /// would be both the obvious answer and the wrong one: that is the BCL's formatting of a
    /// *parsed* <c>AssemblyName</c>, and it is also the key PawPrint registers assemblies
    /// under. It diverges three ways, each confirmed against the real runtime — it normalises
    /// the culture (<c>EN-gb</c> becomes <c>en-GB</c>), it omits
    /// <c>processorArchitecture</c> entirely, and it escapes embedded quotes differently.
    /// </remarks>
    let displayName
        (simpleName : string)
        (version : System.Version)
        (culture : string)
        (publicKey : byte[])
        (flags : int)
        : string
        =
        // `ToString` clears its output and returns the moment the simple name is empty, so
        // such an assembly's display name is the empty string rather than a nameless list of
        // the remaining segments. Unreachable through the real runtime, which refuses to load
        // an image whose `Name` column is empty ("The given assembly name was invalid"), but
        // PawPrint's loader has no such check.
        //
        // Note this is a different situation from `GetSimpleName`, which fails loudly on the
        // same column. There, CoreCLR only yields "" when the metadata import itself failed —
        // a corrupted image by its own assertion — whereas here `ToString` branches on the
        // value deliberately, for a row that parsed perfectly well.
        if System.String.IsNullOrEmpty simpleName then
            ""
        else

        let built = System.Text.StringBuilder ()

        built.Append (escapeDisplayNameSegment simpleName)
        |> ignore<System.Text.StringBuilder>

        // `0xFFFF` in the major column suppresses the whole segment. That is a real value of
        // the row's `USHORT` column rather than an impossible sentinel, so an assembly
        // versioned 65535.x.y.z genuinely has no `Version=` in its display name.
        if version.Major <> 0xFFFF then
            built.Append(", Version=").Append(version.Major).Append('.').Append (version.Minor)
            |> ignore<System.Text.StringBuilder>

            built.Append('.').Append(version.Build).Append('.').Append (version.Revision)
            |> ignore<System.Text.StringBuilder>

        built.Append ", Culture=" |> ignore<System.Text.StringBuilder>

        if System.String.IsNullOrEmpty culture then
            built.Append "neutral" |> ignore<System.Text.StringBuilder>
        else
            built.Append (escapeDisplayNameSegment culture)
            |> ignore<System.Text.StringBuilder>

        // `BaseAssemblySpec::Init` ORs `afPublicKey` in whenever the blob is non-empty, so
        // `IsAfPublicKeyToken` — which is the *absence* of that bit — is never true for a
        // manifest row. The blob is therefore always a key to be hashed, never a token to be
        // copied, and the branch that copies one is unreachable from here.
        if publicKey.Length = 0 then
            built.Append ", PublicKeyToken=null" |> ignore<System.Text.StringBuilder>
        else
            built.Append ", PublicKeyToken=" |> ignore<System.Text.StringBuilder>

            for b in publicKeyToken publicKey do
                built.Append (b.ToString "x2") |> ignore<System.Text.StringBuilder>

        // Lowercase initial, alone among the segments, and that is upstream's spelling.
        match processorArchitectureSegment flags with
        | None -> ()
        | Some architecture ->
            built.Append(", processorArchitecture=").Append architecture
            |> ignore<System.Text.StringBuilder>

        // afRetargetable
        if flags &&& 0x100 <> 0 then
            built.Append ", Retargetable=Yes" |> ignore<System.Text.StringBuilder>

        // afContentType_Mask / afContentType_WindowsRuntime. Tested as a field equality, not a
        // bit test, so the other values of the mask emit nothing. Unreachable through the
        // real runtime, which refuses to *load* a WindowsRuntime assembly at all ("The given
        // assembly name was invalid"), but PawPrint's loader has no such check, so the guest
        // can get here.
        if flags &&& 0xE00 = 0x200 then
            built.Append ", ContentType=WindowsRuntime" |> ignore<System.Text.StringBuilder>

        built.ToString ()

    let private writeLength
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (lengthOut : ManagedPointerSource)
        (length : uint32)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state lengthOut (NativeCall.cliUInt32 length)

    let private assemblyHandleOfRuntimeAssemblyRef
        (operation : string)
        (state : IlMachineState)
        (runtimeAssemblyRef : EvalStackValue)
        : string
        =
        let runtimeAssemblyAddr =
            match runtimeAssemblyRef with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith $"TODO: %s{operation} on null RuntimeAssembly should throw NRE"
            | other -> failwith $"%s{operation}: expected ObjectRef for RuntimeAssembly argument, got %O{other}"

        let heapObj = ManagedHeap.get runtimeAssemblyAddr state.ManagedHeap

        let assemblyField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_assembly"

        match
            AllocatedNonArrayObject.DereferenceFieldById assemblyField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)) ->
            assemblyFullName
        | other -> failwith $"%s{operation}: expected AssemblyHandle in RuntimeAssembly.m_assembly, got %O{other}"

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          ("GetToken" | "GetTokenInternal"),
          [ CorelibType state.ConcreteTypes ("System.Reflection", "RuntimeAssembly", runtimeAssemblyGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeAssemblyGenerics.IsEmpty
            ->
            let operation = "RuntimeAssembly.GetToken"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeAssemblyRef, state = IlMachineState.popEvalStack ctx.Thread state

            assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef |> ignore

            // Every assembly manifest has a single Assembly metadata row.
            let mdAssemblyToken = 0x20000001

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 mdAssemblyToken)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          "GetManifestModule",
          [ CorelibType state.ConcreteTypes ("System.Reflection", "RuntimeAssembly", runtimeAssemblyGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System.Reflection",
                                                                     "RuntimeModule",
                                                                     runtimeModuleGenerics)) when
            runtimeAssemblyGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            let operation = "RuntimeAssembly.GetManifestModule"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeAssemblyRef, state = IlMachineState.popEvalStack ctx.Thread state

            let assemblyFullName =
                assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let runtimeModuleAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeModule
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    assembly.DefinitionFullName
                    state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some runtimeModuleAddr)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None

    /// <summary>
    /// How the QCall's caller encoded the type names it passed.
    /// </summary>
    /// <remarks>
    /// The two entry points differ in their marshalling as well as their casing: the UTF-8 one is
    /// handed a copy in a <c>localloc</c> buffer, the UTF-16 one a pointer pinned into the guest's
    /// own <c>System.String</c>. Both are NUL-terminated, so both are read the same way once the
    /// element width is known.
    /// </remarks>
    [<RequireQualifiedAccess>]
    type private TypeNameEncoding =
        | Utf8
        | Utf16

    /// <summary>
    /// The body shared by <c>AssemblyNative_GetTypeCore</c> and
    /// <c>AssemblyNative_GetTypeCoreIgnoreCase</c>, which are the same walk over
    /// <c>vm/assemblynative.cpp</c>'s loop with the case sensitivity of the lookups swapped.
    /// </summary>
    let private executeGetTypeCore
        (operation : string)
        (encoding : TypeNameEncoding)
        (ignoreCase : bool)
        (ctx : NativeCallContext)
        : NativeHandlerResult option
        =
        let state = ctx.State
        let instruction = ctx.Instruction

        let readName (state : IlMachineState) (ptr : ManagedPointerSource) : string =
            match encoding with
            | TypeNameEncoding.Utf8 -> NativeCall.readNullTerminatedUtf8 operation ctx.BaseClassTypes state ptr
            | TypeNameEncoding.Utf16 -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state ptr

        // A case-insensitive lookup PawPrint will not answer is a hard stop rather than a miss.
        // Answering `null` would be indistinguishable, to the guest, from "no such type" — and the
        // real runtime does find one here, so a silent null is the wrong answer rather than an
        // incomplete one.
        let orRefuse (what : string) (result : Result<'a option, CaseInsensitiveLookupRefusal>) : 'a option =
            match result with
            | Ok found -> found
            | Error refusal -> failwith $"%s{operation}: %s{what}: %O{refusal}"

        // ECMA-335 II.22.37: the first `TypeDef` row is the pseudo-class that parents an assembly's
        // module-scope functions and variables. `ClassLoader::PopulateAvailableClassHashTable`
        // skips it, so it is in no table the class loader searches and `Assembly.GetType` never
        // answers with it — measured: real .NET answers `null` for `<Module>` at either casing,
        // and `GetTypes()` omits it too. Identified by its row rather than its name, because the
        // name is a legal identifier that a hand-written assembly could also give a real type.
        let isModulePseudoType (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : bool =
            System.Reflection.Metadata.Ecma335.MetadataTokens.GetRowNumber (
                System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeInfo.TypeDefHandle
            ) = 1

        let topLevelTypeDef
            (assembly : DumpedAssembly)
            (ns : string)
            (name : string)
            : TypeInfo<GenericParamFromMetadata, TypeDefn> option
            =
            if ignoreCase then
                // The exclusion goes *into* the scan rather than onto its result: two names
                // collide only among the candidates the class loader would have searched, so
                // filtering afterwards could refuse a query as ambiguous with a row that is not a
                // candidate at all.
                assembly.TryGetTopLevelTypeDefIgnoreCase ns name (isModulePseudoType >> not)
                |> orRefuse $"looking up %s{ns}.%s{name} in %s{assembly.Name.Name}"
            else
                // Exact lookup has at most one hit, so there is no collision for the pseudo-row to
                // take part in and filtering the answer is the same thing.
                assembly.TryGetTopLevelTypeDef ns name
                |> Option.filter (isModulePseudoType >> not)

        let topLevelExportedType
            (assembly : DumpedAssembly)
            (ns : string)
            (name : string)
            : WoofWare.PawPrint.ExportedType option
            =
            if ignoreCase then
                assembly.TryGetTopLevelExportedTypeIgnoreCase (Some ns) name
                |> orRefuse $"looking up exported type %s{ns}.%s{name} in %s{assembly.Name.Name}"
            else
                assembly.TryGetTopLevelExportedType (Some ns) name

        let nestedTypeDef
            (assembly : DumpedAssembly)
            (declaring : System.Reflection.Metadata.TypeDefinitionHandle)
            (name : string)
            : TypeInfo<GenericParamFromMetadata, TypeDefn> option
            =
            if ignoreCase then
                assembly.TryGetNestedTypeDefIgnoreCase declaring name
                |> orRefuse $"looking up nested type %s{name} in %s{assembly.Name.Name}"
            else
                assembly.TryGetNestedTypeDef declaring name


        if instruction.Arguments.Length <> 5 then
            failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

        let assemblyFullName =
            instruction.Arguments.[0]
            |> NativeCall.qCallAssemblyToAssemblyFullName operation state

        let typeNamePtr =
            NativeCall.managedPointerOfPointerArgument operation "typeName" instruction.Arguments.[1]

        let nestedNamesPtr =
            NativeCall.managedPointerOfPointerArgument operation "nestedTypeNames" instruction.Arguments.[2]

        let nestedCount = NativeCall.int32Argument operation instruction.Arguments.[3]

        let retType =
            NativeCall.objectHandleOnStackTarget operation state "retType" instruction.Arguments.[4]

        if nestedCount < 0 then
            failwith $"%s{operation}: nested type count %d{nestedCount} is negative"

        match typeNamePtr with
        | ManagedPointerSource.Null ->
            failwith $"TODO: %s{operation} with null typeName should throw ArgumentNullException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read typeName through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ -> ()

        if nestedCount > 0 then
            match nestedNamesPtr with
            | ManagedPointerSource.Null ->
                failwith
                    $"%s{operation}: nestedTypeNames pointer was null but nestedCount=%d{nestedCount} (caller invariant violated)"
            | ManagedPointerSource.NativeIntPlaceholder bits ->
                failwith
                    $"%s{operation}: cannot read nestedTypeNames through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
            | ManagedPointerSource.Byref _ -> ()

        let typeName = readName state typeNamePtr

        let assembly =
            state.LoadedAssembly assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

        let nestedNames =
            if nestedCount = 0 then
                []
            else
                // sizeof<nativeint> matches CoreCLR's IntPtr ABI on the
                // host. PawPrint's interpreter is a 64-bit-only host today.
                let intPtrStride = sizeof<nativeint>

                let byteConcreteType =
                    let h =
                        AllConcreteTypes.findExistingNonGenericConcreteType
                            state.ConcreteTypes
                            ctx.BaseClassTypes.Byte.Identity
                        |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Byte is not concretized")

                    AllConcreteTypes.lookup h state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete System.Byte handle %O{h} not found"
                    )

                [
                    for i in 0 .. nestedCount - 1 do
                        let entryPtr =
                            ManagedPointerByteView.addByteOffset
                                state
                                byteConcreteType
                                (i * intPtrStride)
                                nestedNamesPtr

                        // Read an IntPtr-sized native int from the cell.
                        let entry =
                            IlMachineState.readManagedByrefBytesAs
                                ctx.BaseClassTypes
                                state
                                entryPtr
                                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

                        let stringPtr =
                            NativeCall.managedPointerOfPointerArgument operation $"nestedTypeNames[{i}]" entry

                        yield readName state stringPtr
                ]

        let ns, simple = splitAtLastDot typeName

        // The asking assembly's class loader consults the manifest's `ExportedType` rows when
        // it has no `TypeDef` of that name, so a forwarder is followed here and the answer is
        // the type as the assembly that *defines* it declares it. That assembly is then also
        // where any nested names are looked up: a nested type lives in the same module as its
        // declaring type, so re-following a forwarder chain for it would be meaningless
        // (vm/assemblynative.cpp:387-393 re-roots `pClassLoader` for exactly that reason).
        // A chain that does not arrive is not a crash here. `Assembly.GetType(name,
        // throwOnError: false)` answers `null` for both ways of not arriving, and
        // `throwOnError: true` distinguishes them — so the two are reported differently and
        // CoreLib decides what the guest sees.
        let definedHere = topLevelTypeDef assembly ns simple

        // Under `ignoreCase` these are one table in CoreCLR (`m_pAvailableClassesCaseIns`), not
        // two consulted in order, so a definition here does not get to win by being looked at
        // first — measured: a facade that defines `N.TARGET` and forwards `N.Target` answers a
        // folded query with the *forwarded* type, in preference to its own exact match. Which of
        // two folded matches CoreCLR picks is the same unspecified hash ordering that makes two
        // colliding definitions ambiguous, so this is refused on the same terms.
        let forwardedFromHere =
            if ignoreCase || definedHere.IsNone then
                topLevelExportedType assembly ns simple
            else
                None

        match definedHere, forwardedFromHere with
        | Some definition, Some export when ignoreCase ->
            let definitionName = $"%s{definition.Namespace}.%s{definition.Name}"

            let exportNs = export.Namespace |> Option.defaultValue ""
            let exportName = $"%s{exportNs}.%s{export.Name}"

            failwith
                $"%s{operation}: case-insensitive lookup of %s{ns}.%s{simple} in %s{assembly.Name.Name} is ambiguous between the type it defines (%s{definitionName}) and the one it forwards (%s{exportName})"
        | _ ->

        let miss, state, topLevel =
            match definedHere with
            | Some typeDef -> ForwarderMiss.AnswerNull, state, Some (assembly, typeDef)
            | None ->

            match forwardedFromHere with
            | None -> ForwarderMiss.AnswerNull, state, None
            | Some export ->

            match
                IlMachineTypeResolution.tryResolveTypeFromExport
                    ctx.LoggerFactory
                    assembly
                    export
                    ImmutableArray.Empty
                    state
            with
            | state, ExportedTypeResolution.Forwarded (definingAssembly, forwarded) ->
                // The walk asked the far side for the forwarder row's own spelling, exactly. Under
                // `ignoreCase` that is not the question CoreCLR asked: it carries the fold across
                // the hop, so a target declaring a folded sibling of the row can answer with the
                // sibling instead. Measured: a facade forwarding `N.TARGET` into a target that
                // declares both `N.Target` and `N.TARGET` answers `N.Target`. Ask the folded
                // question too, and stop unless it has the same answer — silently returning a
                // different type from the real runtime is the worst way to be wrong.
                if ignoreCase then
                    let exportNs = export.Namespace |> Option.defaultValue ""

                    match
                        definingAssembly.TryGetTopLevelTypeDefIgnoreCase
                            exportNs
                            export.Name
                            (isModulePseudoType >> not)
                    with
                    | Error refusal ->
                        failwith
                            $"%s{operation}: case-insensitive lookup of %s{ns}.%s{simple} followed a forwarder into %s{definingAssembly.Name.Name}, where the folded name does not have one answer: %O{refusal}"
                    | Ok (Some folded) when folded.TypeDefHandle = forwarded.TypeDefHandle ->
                        // The row's own declaration is the only thing on the far side folding to
                        // it, so the exact walk and the folded question have the same answer.
                        ()
                    | Ok found ->
                        // Defensive, and believed unreachable: the walk arrived *through* a
                        // declaration of the row's own spelling, which necessarily folds to the
                        // folded query too — so anything else that folds alike makes two
                        // candidates and comes back as `Error Ambiguous` above, and a lone
                        // different answer cannot arise. Kept because "cannot arise" is an
                        // argument rather than a check, and stopping is cheap.
                        let foundName =
                            match found with
                            | None -> "nothing"
                            | Some found -> $"%s{found.Namespace}.%s{found.Name}"

                        failwith
                            $"%s{operation}: case-insensitive lookup of %s{ns}.%s{simple} followed a forwarder into %s{definingAssembly.Name.Name}, where the row's own spelling names %s{forwarded.Namespace}.%s{forwarded.Name} but the folded name names %s{foundName}. Folding is not yet carried across a forwarder hop."

                // Back to the type as its own metadata declares it: no generic arguments were
                // supplied above, so nothing was substituted, and the allocation below wants
                // the declared generic parameters rather than an instantiation of them.
                ForwarderMiss.AnswerNull,
                state,
                Some (definingAssembly, definingAssembly.TypeDefs.[forwarded.TypeDefHandle])
            | state, ExportedTypeResolution.TypeAbsent miss when ignoreCase ->
                // The forwarder row matched by folding, and then the walk looked the row's own
                // spelling up *exactly* in the target. Measured: CoreCLR carries the fold across
                // the hop, so where the target declares a differently-cased name it still
                // resolves. Answering `null` here would be answering a question we did not ask.
                failwith
                    $"%s{operation}: case-insensitive lookup of %s{ns}.%s{simple} followed a forwarder out of %s{assembly.Name.Name}, and the target does not declare that name under the forwarder's own spelling: %O{miss}. Folding is not yet carried across a forwarder hop."
            | state, ExportedTypeResolution.TypeAbsent _ ->
                // Every assembly in the chain bound, and none declares the type. CoreCLR's
                // loader hands back a null TypeHandle, and `TypeNameResolver` is what turns
                // that into a `TypeLoadException` when the caller asked to be thrown at — so
                // reporting absence here is reporting it at the right layer.
                ForwarderMiss.AnswerNull, state, None
            | state, ExportedTypeResolution.AssemblyUnavailable reference ->
                // The chain named an assembly nothing supplies. CoreCLR raises
                // `FileNotFoundException` out of the QCall, and `RuntimeAssembly.GetTypeCore`
                // catches it `when (!throwOnFileNotFound)` — so raising it is what lets
                // `throwOnError: false` answer null while `throwOnError: true` throws.
                ForwarderMiss.AssemblyUnavailable reference, state, None
            | state, ExportedTypeResolution.BaseTypeAbsent typeMiss ->
                // The type was found; something in its base chain is not declared where the
                // metadata says. That is a load failure of a *type*, not of an assembly, so the
                // catch in `RuntimeAssembly.GetTypeCore` does not apply and the guest sees it
                // either way.
                ForwarderMiss.BaseTypeAbsent typeMiss, state, None

        let resolved =
            match topLevel with
            | None -> None
            | Some (definingAssembly, top) ->
                let rec walk
                    (parent : TypeInfo<GenericParamFromMetadata, TypeDefn>)
                    (rest : string list)
                    : TypeInfo<GenericParamFromMetadata, TypeDefn> option
                    =
                    match rest with
                    | [] -> Some parent
                    | name :: rest ->
                        // Each nested entry is normally a simple name; keep
                        // the same split-at-last-'.' rule as CoreCLR uses
                        // when consumers smuggle a dotted name through.
                        let _, nestedSimple = splitAtLastDot name

                        match nestedTypeDef definingAssembly parent.TypeDefHandle nestedSimple with
                        | None -> None
                        | Some child -> walk child rest

                walk top nestedNames

        match resolved with
        | None ->
            match miss with
            | ForwarderMiss.AnswerNull ->
                // Caller's local was preinitialized to null (Type? type = null);
                // leaving retType untouched preserves that.
                NativeHandlerResult.completed state |> Some
            | ForwarderMiss.AssemblyUnavailable reference ->
                // `FileName` stays null where the real runtime names the assembly: this channel
                // runs the parameterless constructor and can then set only `_message`.
                //
                // Elsewhere PawPrint declines to write a message it cannot pair with the field
                // that agrees with it (`_paramName`, `_className`), because a half-populated
                // exception invites a guest to trust one half. The reasoning does not reach
                // here: the alternative is not a duller exception but no exception, i.e. the
                // host abort this replaced, which diverges from the real runtime in the
                // *control flow* a guest can actually branch on. Under `throwOnError: false`,
                // the overwhelmingly common caller, CoreLib swallows this whole and neither
                // half is observable at all.
                NativeHandlerResult.raiseExceptionWithMessage
                    ctx.BaseClassTypes.FileNotFoundException
                    (Some $"Could not load file or assembly '%s{reference.Name.FullName}'.")
                    state
                |> Some
            | ForwarderMiss.BaseTypeAbsent typeMiss ->
                // Wording measured off .NET 10, which reports the *base* type's name and the
                // assembly that was supposed to declare it. `TypeName` is that same name there
                // and stays null here, for the reason given on the sibling arm above.
                let message =
                    match typeMiss with
                    | TypeResolutionMiss.TopLevelTypeAbsent (searchedIn, missNs, missName) ->
                        let qualified =
                            match missNs with
                            | None
                            | Some "" -> missName
                            | Some missNs -> $"%s{missNs}.%s{missName}"

                        $"Could not load type '%s{qualified}' from assembly '%s{searchedIn}'."
                    | TypeResolutionMiss.NestedTypeAbsent (searchedIn, declaringType, missName) ->
                        $"Could not load type '%s{declaringType}+%s{missName}' from assembly '%s{searchedIn}'."

                NativeHandlerResult.raiseExceptionWithMessage ctx.BaseClassTypes.TypeLoadException (Some message) state
                |> Some
        | Some typeInfo ->
            let runtimeTypeAddr, state =
                if typeInfo.Generics.IsEmpty then
                    NativeRuntimeType.getOrAllocateNonGenericRuntimeType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        typeInfo
                else
                    // Generic type definition: matches typeof(List<>) — the
                    // RuntimeType represents the open generic, not a
                    // construction. Constructed generics arrive via
                    // Type.MakeGenericType, not here.
                    IlMachineState.getOrAllocateType
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        (RuntimeTypeHandleTarget.OpenGenericTypeDefinition typeInfo.Identity)
                        state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retType
                    (CliType.ObjectRef (Some runtimeTypeAddr))

            NativeHandlerResult.completed state |> Some

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "AssemblyNative_GetResource",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ],
          MethodReturnType.Returns (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) when
            qCallAssemblyGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetResource"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let resourceNamePtr =
                NativeCall.managedPointerOfPointerArgument operation "resourceName" instruction.Arguments.[1]

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length" instruction.Arguments.[2]

            let resourceName =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state resourceNamePtr

            if resourceName.Length = 0 then
                failwith $"TODO: %s{operation} with empty resource name should throw ArgumentException"

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let state =
                match AssemblyApi.findManifestResource assembly resourceName with
                | ManifestResourceLookupResult.NotFound ->
                    let state = writeLength ctx state lengthOut 0u

                    IlMachineState.pushToEvalStack'
                        (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
                        ctx.Thread
                        state
                | ManifestResourceLookupResult.Embedded resource ->
                    let state = writeLength ctx state lengthOut (uint32 resource.PayloadLength)
                    let peByteRange = IlMachineState.peByteRangeForEmbeddedManifestResource resource

                    // Return a pointer even when PayloadLength is zero: null
                    // means "resource not found", while a zero-sized PE range
                    // means "resource exists and is empty".
                    let state, dataPtr =
                        IlMachineState.peByteRangePointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

                    IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer dataPtr) ctx.Thread state
                | ManifestResourceLookupResult.ExternalFile resource ->
                    // Deliberately fail loudly until linked-file resources are
                    // implemented. CoreCLR returns null for manifest resources
                    // stored in separate files.
                    failwith
                        $"TODO: %s{operation} does not support external-file manifest resource %s{resource.Name} in %s{resource.AssemblyFullName} from %s{resource.FileName}"
                | ManifestResourceLookupResult.ReferencedAssembly (actualResourceName, assemblyReference) ->
                    // Deliberately fail loudly until forwarded resources are
                    // implemented. CoreCLR follows the AssemblyRef chain, as
                    // used by satellite/resource-carrier assemblies.
                    failwith
                        $"TODO: %s{operation} does not support assembly-forwarded manifest resource %s{actualResourceName} in %s{assemblyFullName} forwarded to %s{assemblyReference.Name.FullName}"

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetCodeBase",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          // `[return: MarshalAs(UnmanagedType.Bool)]` over a native `BOOL`, so the signature
          // the interpreter sees is Int32-returning rather than Boolean-returning.
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            qCallAssemblyGenerics.IsEmpty && stringHandleGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetCodeBase"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            // Decoded and checked to keep the handler honest about its input, even though
            // the answer below does not depend on which assembly this is.
            state.LoadedAssembly assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")
            |> ignore<DumpedAssembly>

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[1]

            // `PEAssembly::GetCodeBase` takes its `else` branch — set the empty string,
            // return FALSE — for an image that is in a bundle or is external data. PawPrint
            // reports every assembly that way.
            //
            // This is a *narrower* claim than the empty `Location` that
            // `AssemblyNative_GetLocation` reports, not the same one restated: CoreCLR's
            // pathless images do not all behave alike here. An `Assembly.Load(byte[])` image
            // is built by `PEImage::CreateFromByteArray` with a null path but no probe
            // extension, so it is neither bundled nor external and takes the *first* branch,
            // returning TRUE with an empty string. Both shapes report `Location == ""`, so
            // that observation alone does not decide this one; we are choosing the
            // single-file/bundle shape specifically. `docs/divergences.md` records why.
            //
            // Note the string is written on *both* of CoreCLR's branches — `retString.Set`
            // sits outside the `if` — so a false return does not mean "left untouched". The
            // managed wrapper discards the written value and returns null when the bool is
            // false, which is what makes `AssemblyName.CodeBase` null and the public
            // `Assembly.CodeBase` throw `NotSupportedException`, exactly as for a
            // single-file-published app.
            let emptyAddr, state =
                IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some emptyAddr))

            // FALSE. The BCL marshals this back to `bool`, so any non-zero value would read
            // as "there is a code base" and hand the guest the empty string as if it were one.
            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        // Declared on `Assembly` itself rather than `RuntimeAssembly` — the `LibraryImport`
        // lives in `Assembly.CoreCLR.cs`, next to `GetEntryAssemblyInternal` which is its only
        // caller.
        | "AssemblyNative_GetEntryAssembly",
          "System.Private.CoreLib",
          "System.Reflection",
          "Assembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetEntryAssembly"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let retAssembly =
                NativeCall.objectHandleOnStackTarget operation state "retAssembly" instruction.Arguments.[0]

            // CoreCLR leaves `retAssembly` untouched when the AppDomain has no root assembly —
            // it was hosted rather than launched from an image — and `GetEntryAssemblyInternal`
            // preinitializes its local to null so that reads back as `null`. PawPrint is only
            // ever entered through an entry assembly (`IlMachineState.initial` demands one), so
            // that branch does not arise here and the write below is unconditional.
            let assembly =
                state.LoadedAssembly state.EntryAssembly.FullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: entry assembly %s{state.EntryAssembly.FullName} is not loaded"
                )

            let runtimeAssemblyAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeAssembly
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    assembly.DefinitionFullName
                    state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retAssembly
                    (CliType.ObjectRef (Some runtimeAssemblyAddr))

            NativeHandlerResult.completed state |> Some
        // Declared on `System.Runtime.Loader.AssemblyLoadContext` rather than on an assembly type:
        // it asks about the load context as a whole. `AppDomain.GetAssemblies()` is the only route
        // a guest has to it today: the one other managed caller, `AssemblyLoadContext.Assemblies`,
        // filters each result through `GetLoadContext`, whose QCall is not implemented.
        | "AssemblyNative_GetLoadedAssemblies",
          "System.Private.CoreLib",
          "System.Runtime.Loader",
          "AssemblyLoadContext",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetLoadedAssemblies"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let retAssemblies =
                NativeCall.objectHandleOnStackTarget operation state "retAssemblies" instruction.Arguments.[0]

            // Load order, not the dictionary's enumeration order — that one is a hash order over
            // per-process-randomised string hashes, so handing it to a guest would make a replay
            // depend on the process that produced it. See `DefinitionNames`.
            let loadOrder = state._LoadedAssemblies.DefinitionNamesInLoadOrder

            // The two are denormalised views of one fact, and only the dictionary is consulted
            // everywhere else, so a registration path that updated one and not the other would
            // show up here as an assembly silently missing from the guest's array rather than as
            // any failure. Cheap to check at the one place the order is read.
            let definitionCount = state._LoadedAssemblies.DefinitionNames |> Seq.length

            if loadOrder.Length <> definitionCount then
                failwith
                    $"%s{operation}: the load context holds %d{definitionCount} definition identities but records %d{loadOrder.Length} in load order. This is an interpreter bug."

            // `AllocateObjectArray(nArrayElems, CoreLibBinder::GetClass(CLASS__ASSEMBLY))`, and
            // `CLASS__ASSEMBLY` is `System.Reflection.RuntimeAssembly` (vm/corelib.h:133) rather
            // than the `Assembly` the managed wrapper's local is typed as. Measured on real .NET:
            // `AppDomain.CurrentDomain.GetAssemblies().GetType()` reports
            // `System.Reflection.RuntimeAssembly[]`.
            let state, _, runtimeAssemblyElementHandle =
                NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    "System.Reflection"
                    "RuntimeAssembly"

            // `GetExposedObject()` is cached per assembly, so an element here is the very object
            // `Assembly.GetExecutingAssembly()` and `typeof(T).Assembly` report, not an equal one.
            let state, elementAddrs =
                loadOrder
                |> Seq.fold
                    (fun (state, acc) assemblyFullName ->
                        let addr, state =
                            NativeRuntimeType.getOrAllocateRuntimeAssembly
                                ctx.LoggerFactory
                                ctx.BaseClassTypes
                                assemblyFullName
                                state

                        state, addr :: acc
                    )
                    (state, [])

            let elementAddrs = List.rev elementAddrs

            // CoreCLR's trailing-null trim cannot arise here. It exists because another thread can
            // load an assembly between the count and the fill, and PawPrint's QCall runs to
            // completion within one scheduling step; the array is allocated at the length it is
            // filled to.
            let arrayAddr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero runtimeAssemblyElementHandle)
                    (fun () -> CliType.ObjectRef None)
                    loadOrder.Length
                    state

            let state =
                elementAddrs
                |> List.indexed
                |> List.fold
                    (fun state (i, addr) ->
                        IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some addr)) i state
                    )
                    state

            // Written unconditionally: `GetLoadedAssemblies` starts from `null` and returns
            // `assemblies!` (AssemblyLoadContext.CoreCLR.cs:34-39), so a skipped write is a
            // NullReferenceException in the guest.
            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retAssemblies
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        // Also declared on `Assembly` rather than on `RuntimeAssembly`: which assembly the answer
        // names is what the stack crawl decides, so there is no assembly to hang the QCall off.
        | "AssemblyNative_GetExecutingAssembly",
          "System.Private.CoreLib",
          "System.Reflection",
          "Assembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StackCrawlMarkHandle",
                                             stackMarkGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when stackMarkGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetExecutingAssembly"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let markPtr =
                NativeCall.stackCrawlMarkHandleTarget operation state "stackMark" instruction.Arguments.[0]

            let retAssembly =
                NativeCall.objectHandleOnStackTarget operation state "retAssembly" instruction.Arguments.[1]

            let threadState = state.ThreadState.[ctx.Thread]

            // CoreCLR locates the marked frame by comparing the mark's address against each frame's
            // stack pointer. PawPrint's byref names that frame outright — but only for the exact
            // shape CoreLib constructs, `new StackCrawlMarkHandle(ref someLocal)`. A projected
            // pointer, an argument slot, or another thread's frame all mean the handle did not come
            // from there, and guessing which frame was meant would be worse than stopping.
            let markFrame =
                match markPtr with
                | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (sourceThread, markFrame, _), []) ->
                    if sourceThread <> ctx.Thread then
                        failwith
                            $"%s{operation}: the stack-crawl mark is stored in a frame of %O{sourceThread}, but the QCall is executing on %O{ctx.Thread}. A stack crawl can only answer about the crawling thread."

                    markFrame
                | other ->
                    failwith
                        $"%s{operation}: expected StackCrawlMarkHandle._ptr to be an unprojected byref to a local variable, got %O{other}"

            // Every mark CoreLib builds is a local of a frame this QCall was reached through, so a
            // decoded frame that is not on the return chain means the handle was misread. Worth
            // checking rather than trusting: on every input a guest can produce today, the frames
            // between the mark and here are all skipped as reflection infrastructure, so a misread
            // frame would still yield the right assembly and the mistake would not show up in a test.
            if not (StackCrawlMark.isOnReturnChainOf markFrame threadState.ActiveMethodState threadState) then
                failwith
                    $"%s{operation}: the stack-crawl mark decoded to %O{markFrame}, which is not this QCall's frame nor one of its callers. This is an interpreter bug."

            let mark =
                match
                    IlMachineState.readManagedByref ctx.BaseClassTypes state markPtr
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.Int32 value) -> StackCrawlMark.ofInt32 operation value
                | other -> failwith $"%s{operation}: expected StackCrawlMark storage to hold an Int32, got %O{other}"

            let caller = StackCrawlMark.resolveCaller operation mark markFrame threadState

            // `DeclaringAssemblyFullName` is total: a dynamic method answers with the assembly it is
            // scoped to, which is what CoreCLR's `pFunc->GetModule()->GetAssembly()` reports for one.
            let runtimeAssemblyAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeAssembly
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    caller.ExecutingMethod.DeclaringAssemblyFullName
                    state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retAssembly
                    (CliType.ObjectRef (Some runtimeAssemblyAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetLocation",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetLocation"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            // Decoded and checked only to keep the handler honest about its
            // input: a caller handing us an assembly we have not loaded is a
            // bug worth hearing about, even though the answer below does not
            // depend on which assembly this is.
            state.LoadedAssembly assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")
            |> ignore<DumpedAssembly>

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[1]

            // CoreCLR answers `pAssembly->GetPEAssembly()->GetPath()`, which is
            // empty for any assembly with no file backing — a byte-array load, a
            // dynamic assembly, or a single-file-published app. Under PawPrint
            // *every* assembly is that shape: the guest has no filesystem, so
            // there is no path it could open. Synthesising a plausible-looking
            // path would be a fiction the guest cannot act on, and for a
            // framework assembly resolved from `DotnetRuntimeDirs` it would also
            // leak the host machine's layout into the run's replay contract.
            //
            // `internCanonicalEmptyString` rather than a fresh allocation because
            // CoreCLR's `StringObject::NewString` returns the shared empty-string
            // instance for a zero-length string, so `ReferenceEquals(asm.Location,
            // string.Empty)` holds there and must hold here.
            let emptyAddr, state =
                IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some emptyAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetFlags",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System.Reflection",
                                                                     "AssemblyNameFlags",
                                                                     flagsGenerics)) when
            qCallAssemblyGenerics.IsEmpty && flagsGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetFlags"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // CoreCLR returns `pAssembly->GetPEAssembly()->GetFlags()`, the whole `DWORD` of
            // the manifest row's `Flags` column. Note `DumpedAssembly.Flags` rather than
            // `Name.Flags`: the latter is a masked view that drops the ContentType and
            // ProcessorArchitecture bits, whereas the managed caller assigns this result to
            // `AssemblyName.RawFlags`, which keeps them.
            //
            // Unlike the rest of this family the value comes back as a return value rather
            // than through an out-parameter. `AssemblyNameFlags` is an Int32-backed enum, so
            // it travels on the eval stack as its underlying primitive.
            //
            // The column is not quite the answer on its own. `GetAssemblyProps` — the
            // metadata-import call every CoreCLR reader of this column goes through —
            // synthesises the `afPublicKey` bit whenever the `PublicKey` blob is non-empty,
            // whatever the column says ("Turn on the afPublicKey if PublicKey blob is not
            // empty", mdinternalro.cpp). The two disagree only for an image whose blob and
            // flag were written inconsistently, which no compiler emits but the format
            // permits, so reproduce the normalisation rather than the column.
            //
            // It lives here rather than on `DumpedAssembly.Flags`: that member is the
            // manifest column and should stay exactly that, while this is the behaviour of
            // one particular CoreCLR API.
            let afPublicKey = 0x0001

            let flags =
                if assembly.PublicKey.IsEmpty then
                    int assembly.Flags
                else
                    int assembly.Flags ||| afPublicKey

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 flags)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetIsCollectible",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("", "BOOL", boolGenerics)) when
            qCallAssemblyGenerics.IsEmpty && boolGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetIsCollectible"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            // CoreCLR is `pAssembly->GetAssembly()->IsCollectible()`. The assembly is resolved
            // rather than ignored so that a handle naming something unloaded is a loud failure
            // here as it is in the rest of this family, even though PawPrint's answer does not
            // depend on which assembly it is: with one loader allocator it cannot.
            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            state.LoadedAssembly assemblyFullName
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")
            |> ignore<DumpedAssembly>

            // Interop.BOOL is int-backed with FALSE = 0 and TRUE = 1.
            let state =
                let ret =
                    if LoaderAllocator.isCollectible LoaderAllocator.Global then
                        1
                    else
                        0

                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetHashAlgorithm",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics) ],
          MethodReturnType.Returns (CorelibType state.ConcreteTypes ("System.Configuration.Assemblies",
                                                                     "AssemblyHashAlgorithm",
                                                                     hashAlgorithmGenerics)) when
            qCallAssemblyGenerics.IsEmpty && hashAlgorithmGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetHashAlgorithm"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // CoreCLR returns `pAssembly->GetPEAssembly()->GetHashAlgId()`, which is the
            // `HashAlgId` column of the manifest row read through `GetAssemblyProps`. Unlike
            // the `Flags` column next door, that call applies no normalisation to this one —
            // it is `*pulHashAlgId = getHashAlgIdOfAssembly(pRecord)` and nothing else
            // (mdinternalro.cpp) — so the column verbatim is the whole answer.
            //
            // Like `GetFlags` and unlike the rest of this family the value is a return value
            // rather than an out-parameter. `AssemblyHashAlgorithm` is an Int32-backed enum,
            // so it travels on the eval stack as its underlying primitive; the column itself
            // is a `ULONG`, but the QCall's own signature is `INT32` and CoreCLR does the
            // same reinterpretation.
            let state =
                IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 (int assembly.HashAlgorithm)))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetLocale",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetLocale"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[1]

            // CoreCLR answers `pAssembly->GetPEAssembly()->GetLocale()`, which is
            // `md.szLocale` — the manifest row's `Culture` column verbatim. Note
            // `DumpedAssembly.CultureName` rather than `Name.CultureName`: the latter
            // has been through `CultureInfo` normalisation and would hand the guest
            // "en-GB" for a column reading "EN-gb".
            let locale = assembly.CultureName

            // CoreCLR guards its write with `if (pLocale)`, leaving the caller's
            // preinitialised `string? locale = null` in place when the pointer is
            // null. That cannot happen for an image we could have loaded: the
            // pointer comes from the `#Strings` heap, and a nil `Culture` index
            // resolves to the empty string there rather than to null (a bad index
            // makes `GetAssemblyProps` fail, which `PEAssembly::GetLocale` throws
            // on). So the write below is unconditional, and a culture-neutral
            // assembly takes the guest's `CultureInfo.GetCultureInfo("")` path —
            // which yields the invariant culture — rather than its `locale == null`
            // fallback, exactly as on CoreCLR.
            let localeAddr, state =
                if System.String.IsNullOrEmpty locale then
                    // `StringObject::NewString` returns the shared empty-string
                    // instance for a zero-length string; see AssemblyNative_GetLocation.
                    IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state
                else
                    IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes locale state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some localeAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetPublicKey",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetPublicKey"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let retPublicKey =
                NativeCall.objectHandleOnStackTarget operation state "retPublicKey" instruction.Arguments.[1]

            // CoreCLR answers `pAssembly->GetPEAssembly()->GetPublicKey(&cb)`, which is the
            // `PublicKey` blob column of the manifest row — the full key, not the eight-byte
            // token a display name carries. Note `DumpedAssembly.PublicKey` rather than
            // `Name.GetPublicKey()`: the latter reports null for an assembly with no key,
            // where the column is a zero-length blob.
            let publicKey = assembly.PublicKey.AsSpan().ToArray ()

            // `ObjectHandleOnStack::SetByteArray` allocates and writes unconditionally —
            // unlike `GetLocale`, there is no null guard here at all — so a zero-length key
            // still produces a real `byte[0]`, and the caller's preinitialised
            // `byte[]? publicKey = null` is always overwritten. A guest asking an
            // unsigned assembly for its key gets an empty array, never null.
            let arrayAddr, state =
                NativeCall.allocateManagedByteArray ctx.BaseClassTypes publicKey state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retPublicKey
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetSimpleName",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetSimpleName"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let retSimpleName =
                NativeCall.stringHandleOnStackTarget operation state "retSimpleName" instruction.Arguments.[1]

            // CoreCLR answers `pAssembly->GetPEAssembly()->GetSimpleName()`, which
            // reads the `Name` column of the manifest's single `Assembly` metadata
            // row (`GetAssemblyProps(TokenFromRid(1, mdtAssembly), ...)`) — *not*
            // the file name, and not a prefix of the display name. PawPrint's
            // `DumpedAssembly.Name` is `AssemblyDefinition.GetAssemblyName()` over
            // that same row, so its `Name` is the same string by construction.
            //
            // The distinction matters: the assembly is keyed here by its *full*
            // name, which additionally carries version, culture and public key
            // token. Splitting that display name back apart would have to undo
            // ECMA-335's quoting of simple names containing ',' or '"', so read
            // the metadata field rather than reparsing.
            let simpleName = assembly.Name.Name

            // CoreCLR only yields "" for an image whose metadata import failed —
            // which its own `_ASSERTE` calls a corrupted image — so an empty or
            // absent name here means we mis-parsed the manifest, not that the
            // guest asked something unusual.
            if System.String.IsNullOrEmpty simpleName then
                failwith $"%s{operation}: assembly %s{assemblyFullName} has no simple name in its Assembly metadata row"

            // `StringObject::NewString` hands back the shared empty-string instance
            // for a zero-length string, but every other length allocates afresh:
            // CoreCLR does not intern QCall results, so two `GetSimpleName` calls
            // on one assembly return reference-distinct strings there and here.
            let nameAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes simpleName state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retSimpleName
                    (CliType.ObjectRef (Some nameAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetFullName",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetFullName"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[1]

            // CoreCLR builds this from the manifest row and nothing else:
            // `PEAssembly::GetDisplayName` seeds an `AssemblySpec` from the row
            // (`BaseAssemblySpec::Init` over `GetAssemblyProps`) and renders it with
            // `ASM_DISPLAYF_FULL`. So the same five raw columns the rest of this family
            // reads one at a time, assembled — deliberately not `assembly.DefinitionFullName`,
            // which is the BCL's rendering of a parsed `AssemblyName` and diverges from this
            // in three ways; see `displayName`.
            //
            // That it is also the key this assembly is registered under makes the
            // distinction easy to lose: returning the key would agree on every assembly a
            // compiler emits and disagree on the ones that matter.
            let version = assembly.Name.Version

            if isNull version then
                failwith $"%s{operation}: assembly %s{assemblyFullName} has no version in its Assembly metadata row"

            let fullName =
                displayName
                    assembly.Name.Name
                    version
                    assembly.CultureName
                    (Array.ofSeq assembly.PublicKey)
                    (int assembly.Flags)

            // `StringHandleOnStack::Set` goes through `StringObject::NewString`, which returns
            // the shared empty-string instance for a zero-length string and allocates afresh
            // for every other length. So an assembly whose display name is empty — the
            // empty-simple-name case above — must hand back the canonical instance, or
            // `ReferenceEquals(asm.FullName, string.Empty)` would answer differently here from
            // there.
            let nameAddr, state =
                if fullName.Length = 0 then
                    IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state
                else
                    IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes fullName state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some nameAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetVersion",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetVersion"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // CoreCLR reads the four `USHORT` columns of the manifest's single
            // `Assembly` metadata row (`PEAssembly::GetVersion` ->
            // `GetAssemblyProps(TokenFromRid(1, mdtAssembly), ..., &md, ...)`) and
            // widens each to `INT32`. PawPrint's `DumpedAssembly.Name` is
            // `AssemblyDefinition.GetAssemblyName()` over that same row, and its
            // `Version` is built from those same four columns, so reading it here
            // is the same four numbers by construction.
            let version = assembly.Name.Version

            if isNull version then
                failwith $"%s{operation}: assembly %s{assemblyFullName} has no version in its Assembly metadata row"

            // `AssemblyName.Version` from metadata is always four-component, because
            // the metadata row always carries all four columns. A `System.Version`
            // built with fewer reports -1 for the missing tail, which would be a
            // value CoreCLR can never produce here (its columns are unsigned), so
            // treat it as a parse bug rather than widening it into the guest.
            let components =
                [
                    "major", version.Major
                    "minor", version.Minor
                    "build", version.Build
                    "revision", version.Revision
                ]

            for name, value in components do
                if value < 0 then
                    failwith
                        $"%s{operation}: assembly %s{assemblyFullName} has no %s{name} version component (got %d{value})"

                // The metadata columns are `USHORT`, so CoreCLR's widening to `INT32`
                // can never exceed `UInt16.MaxValue`. Anything larger means we read
                // the version from somewhere other than the Assembly row.
                if value > int System.UInt16.MaxValue then
                    failwith
                        $"%s{operation}: assembly %s{assemblyFullName} has %s{name} version component %d{value}, which does not fit the metadata row's UInt16 column"

            let writeComponent (argIndex : int) (argName : string) (value : int) (state : IlMachineState) =
                let target =
                    NativeCall.managedPointerOfPointerArgument operation argName instruction.Arguments.[argIndex]

                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    target
                    (CliType.Numeric (CliNumericType.Int32 value))

            let state =
                state
                |> writeComponent 1 "majVer" version.Major
                |> writeComponent 2 "minVer" version.Minor
                |> writeComponent 3 "buildNum" version.Build
                |> writeComponent 4 "revNum" version.Revision

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetModules",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            // `AssemblyNative_GetModules` (coreclr/vm/assemblynative.cpp:696), behind
            // `Assembly.GetModules()`, `GetModules(bool)` and `GetLoadedModules(bool)`. CoreCLR
            // appends the assembly's own module, walks the `File` table calling
            // `Module::LoadModule` on each row when `fLoadIfNotFound`, and writes back an array of
            // each module's `GetExposedObject()`.
            //
            // The two `Int32` parameters are the `[MarshalAs(UnmanagedType.Bool)] bool` ones after
            // the `LibraryImport` stub has lowered them; that stub is `brtrue`-based, so only 0
            // and 1 arrive here.
            let operation = "AssemblyNative_GetModules"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let loadIfNotFound =
                NativeCall.int32Argument operation instruction.Arguments.[1] <> 0

            // `fGetResourceModules` is read nowhere in CoreCLR's function body, so a resource-only
            // `File` row is treated exactly as a code module row would be. Measured against real
            // .NET rather than inferred from the parameter's name: `GetModules(false)` on an
            // assembly built with `csc /linkresource` throws just as `GetModules(true)` does.
            NativeCall.int32Argument operation instruction.Arguments.[2] |> ignore<int>

            let retModules =
                NativeCall.objectHandleOnStackTarget operation state "retModuleHandles" instruction.Arguments.[3]

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // A `File` row can only ever end the call: `ModuleBase::LoadModule`
            // (vm/ceeload.cpp:2543) reads the row's name and then unconditionally throws
            // `COR_E_MULTIMODULEASSEMBLIESDIALLOWED`, so CoreCLR never appends a second module. It
            // *is* a virtual with an override that returns one (vm/readytoruninfo.cpp:1777), but
            // that override is `final` on `NativeManifestModule` — the synthetic metadata module of
            // a composite ReadyToRun image, never what `pAssembly->GetModule()` returns.
            //
            // Refused here rather than reproduced as a guest-catchable `FileLoadException`. That
            // exception carries a `FileName` (the *row's* name, not the assembly's) and
            // `HResult = 0x8013101E`, and the raise machinery available to a native handler can
            // set neither: it calls the parameterless ctor and overwrites `_message`, which would
            // leave a guest reading `null` and `0x80131621`. Two silently wrong guest-readable
            // values is a worse trade than a loud stop.
            if loadIfNotFound then
                let metadataReader =
                    System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader assembly.PeReader

                let files = metadataReader.AssemblyFiles

                if files.Count > 0 then
                    // Named after the row CoreCLR's exception would name: `EnumNext` yields rids
                    // ascending and `LoadModule` throws on the first row it is handed, so it is
                    // always row 1 that is reported however many rows there are.
                    let firstFile =
                        (files :> seq<System.Reflection.Metadata.AssemblyFileHandle>)
                        |> Seq.head
                        |> metadataReader.GetAssemblyFile

                    failwith
                        $"%s{operation}: assembly %s{assembly.DefinitionFullName} has %d{files.Count} File row(s), the first naming %s{metadataReader.GetString firstFile.Name}, and loadIfNotFound was set; multi-module and linked-file assemblies are not supported"

            let moduleAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeModule
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    assembly.DefinitionFullName
                    state

            // `AllocateObjectArray(modules.GetCount(), CoreLibBinder::GetClass(CLASS__MODULE))`,
            // and `CLASS__MODULE` is `System.Reflection.RuntimeModule` (vm/corelib.h:615) —
            // matching the `RuntimeModule[]` local `GetModulesInternal` declares, which the public
            // `Assembly.GetModules(bool)` only widens to `Module[]` on the way out.
            let state, _, runtimeModuleElementHandle =
                NativeRuntimeTypeHelpers.concretizeNonGenericCorelibType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    "System.Reflection"
                    "RuntimeModule"

            // Freshly allocated per call, while the module object it holds is cached — so two
            // `GetModules()` calls give a guest two distinct arrays holding the same element, and
            // that element is the one `Assembly.ManifestModule` reports.
            let arrayAddr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero runtimeModuleElementHandle)
                    (fun () -> CliType.ObjectRef None)
                    1
                    state

            let state =
                IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some moduleAddr)) 0 state

            // Written unconditionally: `GetModulesInternal` starts from `null` and returns
            // `modules!` (RuntimeAssembly.cs:678-685), so a skipped write is a
            // NullReferenceException in the guest.
            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retModules
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        | "AssemblyNative_GetTypeCore",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            executeGetTypeCore "AssemblyNative_GetTypeCore" TypeNameEncoding.Utf8 false ctx
        | "AssemblyNative_GetTypeCoreIgnoreCase",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallAssembly", qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            executeGetTypeCore "AssemblyNative_GetTypeCoreIgnoreCase" TypeNameEncoding.Utf16 true ctx
        | _ -> None
