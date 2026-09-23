namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter, which is where essentially all of the suite's
// time goes; `Explicit` keeps it out of a bare `dotnet test` so local iteration is
// quick. CI selects it by category and so runs it. See AGENTS.md.
[<Category("Guest")>]
[<Explicit>]
module TestPureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            "CustomAttributeTypeArgNested.cs" // A `System.Type`-valued attribute argument naming a nested type of the decorated assembly, "Outer+Inner". CoreLib's `TypeNameResolver` splits the name and calls `RuntimeAssembly.GetTypeCore(string, ReadOnlySpan<string> nestedTypeNames, ...)`, a `LibraryImport` whose generated stub marshals the span into a stack-allocated pointer array, and the interpreted stub stops at "MemoryBlock.readBytes: byte at offset 0 in <stack memory block #0> is uninitialised" (in `RuntimeAssembly.GetTypeCore` at IL offset 110). Measured: a guest calling `Type.GetType("Outer+Inner")` directly stops at the same instruction, so this is the nested-name path of `GetTypeCore`, not anything in the attribute decoder; `sourcesPure/CustomAttributeTypeArg.cs` covers the top-level names, which take the empty-span path. Un-park when that stub's stack buffer reads back what it wrote. Verified to exit 0 on real .NET.
            "AssemblyGetTypeNested.cs" // `Assembly.GetType("Outer+Inner")`, and a nested type whose base lives in a framework assembly nothing else loads, which `AssemblyGetTypeUnloadedBaseAssembly.cs` cannot cover for the nested walk. It stops before the QCall, at the same nested-name marshalling gap as `CustomAttributeTypeArgNested.cs`: "MemoryBlock.readBytes: byte at offset 0 in <stack memory block #0> is uninitialised" (in `RuntimeAssembly.GetTypeCore` at IL offset 110), measured with the nested type's base plain `object` too. Un-park with that one. Verified to exit 0 on real .NET.
            "CustomAttributeTypeArgForeign.cs" // A `System.Type`-valued attribute argument naming a type from another assembly. Roslyn writes every such name assembly-qualified -- `typeof(int)` included, measured as "System.Int32, System.Runtime, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a" against the reference pack -- and CoreLib's `TypeNameResolver` then binds that assembly through `RuntimeAssembly.InternalLoad`, so the guest stops at "Unimplemented native method (PInvokeImpl QCall!AssemblyNative_InternalLoad)". The resolution itself is CoreLib's own code, reached from `CustomAttribute_CreateCustomAttributeInstance` exactly as CoreCLR reaches it, so nothing in the attribute path is what is missing; `sourcesPure/CustomAttributeTypeArg.cs` covers the names Roslyn leaves unqualified, which are the decorated assembly's own types. Un-park when `AssemblyNative_InternalLoad` binds a display name. Verified to exit 0 on real .NET.
            "UnsafeAccessorTypeByName.cs" // `[UnsafeAccessorType("...")]` names a parameter's or the return's type by string rather than in the signature, which is how CoreLib's own accessors reach `BinaryFormatter`, `GenericPrincipal` and `MetadataReader` -- 18 of the 20 accessor declarations in the shared framework are of this shape. `MethodInfo.read` detects the attribute (`hasTypeNameOverrides`), and dispatch refuses on it rather than resolving the member against the signature's `System.Object`: "TODO: [UnsafeAccessor] .TestUnsafeAccessorTypeByName::NewHidden names at least one of its types with [UnsafeAccessorType]". Un-park when the attribute's blob is parsed and its type name resolved -- note that the name may be assembly-qualified, that an unqualified one means the accessor's own assembly (the form this file uses), and that CoreCLR emits a type check on every translated argument (`EmitTypeCheck`). Verified to exit 0 on real .NET.
            "ValueTypeHashCodeOverlappingReferenceFields.cs" // Explicit layout may put two *reference* fields at the same offset -- the GC sees one pointer slot, so the type loads, and real .NET hashes whichever object the slot holds (measured: exits 0). PawPrint cannot reach it, and not because of anything in `ValueType_GetHashCodeStrategy`: an explicit-layout struct with overlapping reference fields has no access route at all. `CliValueType.DereferenceFieldById` sees more than one field covering the range and falls back to rendering the bytes, and `CliType.ToBytes` refuses a non-null reference because PawPrint models one as an opaque handle rather than an address. That is the same dead end as `RuntimeHelpersBoxReferenceContainingStruct.cs`, `ReinterpretCellUnderAliasedAncestor.cs` and `BulkMoveAcrossOverlappedStructPadding.cs`. Note that answering the strategy's nullness question some other way would not be enough: the guest's own `Unsafe.As<byte, object>(ref rawData + fieldOffset)` read then hits the same wall, because the two aliased cells make `tryReadHeapValueFieldPrecise`'s uniqueness gate fail and the byte walk refuses a reference-containing payload. Un-park when a reference cell can be named through an aliased explicit-layout offset. Verified to exit 0 on real .NET.
            "BulkMoveAcrossOverlappedStructPadding.cs" // A bulk move across padding that *two* fields cover, which explicit layout produces by overlaying identical reference-containing structs. `CliType.TryPaddingRunAt` refuses such a byte — with two fields over it there is no single one to descend through, so it cannot say whose padding it is — and that refusal is the one gap left in the padding step that `BulkMoveAcrossStructPadding.cs` covers. Parked because the refusal is currently unreachable, and measured rather than assumed: an explicit-layout struct with any overlap is stored byte-backed, a byte-backed value holding references cannot be field-accessed, and so the plain `src[i].First.N = i + 1` that *builds* the array stops in `CliType.OfBytesLike` with "non-primitive template ObjectRef None" before any copy happens. Allocating the array alone succeeds; it is the first field write that fails. Same blocker as `ReinterpretCellUnderAliasedAncestor.cs`; un-park when that lands, at which point `TryPaddingRunAt`'s two-fields-cover-it branch is first exercised and will need to learn that padding shared by fields which are padding there too is still padding.
            "ReinterpretCellUnderAliasedAncestor.cs" // A named cell under an explicit-layout ancestor that an unrelated sibling overlaps. Parked on a gap well below cell naming: an explicit-layout struct with any overlap is stored byte-backed, so `CliValueType.DereferenceFieldById` rebuilds a field via `OfBytesLike`, which refuses non-primitive templates — so reference-containing explicit-layout structs cannot be field-accessed at all. The program fails at a plain `outer.Whole.R = ...` before reaching any reinterpret. Not a regression: that path is untouched by the cell resolver.
            "ReinterpretSameWidthReadFromNamedCell.cs" // The read mirror of `ReinterpretSameWidthStoreIntoNamedCell.cs`: `Unsafe.As<byte, sbyte>(ref buffer[k].U8)` read by `ldind.i1` over an `[InlineArray]` slot whose element holds a reference. The storage has no byte image, so the read must name the cell, and the read-side naming (`tryNameCellForByrefAccess`, strict `isCellIdentityCompatible`) refuses an `Int8` template for the `UInt8` cell, so the read falls to `resolveCell`. Measured: "refusing byte view over value type containing object references in single-cell byref <<field ...::U8 ...> as System.SByte>". The write side names such a cell by extent and splices the payload's bytes into it (`tryNameCellForByteWrite`); un-park when a read that names a byte-addressable cell decodes that cell's bytes as the template. Verified to exit 0 on real .NET.
            "MakeGenericMethodOpenArgument.cs" //`RuntimeMethodHandle_GetStubIfNeededSlow` (issue #743) handles `MakeGenericMethod` with closed type arguments, which is what every reachable path needs, but an argument that still contains generic parameters — `MakeGenericMethod(typeof(G<>))` or `MakeGenericMethod(someTypeParameter)` — cannot be represented. Both are legal: real .NET returns a MethodInfo with `ContainsGenericParameters = true`, inspectable but not invokable. PawPrint's `MethodHandle.MethodGenerics` is a `ConcreteTypeHandle list`, and `ConcreteTypeHandle` indexes `AllConcreteTypes`, whose entries carry only *closed* generic arguments, so the QCall fails with a precise TODO. Widening that representation reaches concretization and every other MethodHandle consumer, so it is its own change rather than part of the QCall.
            "ReflectionInvokeConstructorOnInstanceManyArguments.cs" // `ConstructorInfo.Invoke(instance, args)` with more than four arguments, the only route to the five-argument `MethodBaseInvoker.InvokeConstructorWithoutAlloc` (MethodBaseInvoker.Constructor.cs:15). It stops before the QCall, in `MethodBaseInvoker.CopyBack`: "MemoryBlock.readBytes: byte at offset 0 in <stack memory block #0> is uninitialised" (measured). That overload's `shouldCopyBack` is a bare `stackalloc bool[argCount]` with no `NativeMemory.Clear`, unlike its `InvokeWithManyArgs` sibling, which clears a block covering all three regions (MethodBaseInvoker.cs:238-243) and so passes. CoreLib is compiled `[module: SkipLocalsInit]`, so `localloc` really does leave that block uninitialised on CoreCLR too, and `CheckArguments` writes `shouldCopyBack[i]` only on the conversion paths (MethodBaseInvoker.cs:354-386) — an argument already of the signature's type leaves its slot untouched. Real .NET therefore reads the garbage and is saved by the copy-back being a no-op (`copyOfParameters[i]` is the argument it was handed); PawPrint's stack memory refuses to be read before it is written. Un-parking means deciding what an uninitialised `localloc` byte reads as, which is a question about the memory model rather than about reflection. Verified to exit 0 on real .NET.
            "ReflectionInvokeByRefReferenceStruct.cs" // `MethodBase.Invoke` with a `ref` parameter whose element is a struct holding a reference. It never reaches the byref: `MethodBaseInvoker.CheckArguments` first copies the boxed argument with `RuntimeHelpers.Box(ref GetRawData(box), handle)` (`TryByRefFastPath`, MethodBaseInvoker.cs:391), and `BoxCache.Box`'s bytewise copy of the source stops at "boxed value byte-view read: refusing byte view over boxed value type containing object references" (measured), the same gap `RuntimeHelpersBoxReferenceContainingStruct.cs` is parked on from a local. Un-park when boxing a reference-holding struct from raw data lands. Verified to exit 0 on real .NET.
            "RuntimeHelpersBoxReferenceContainingStruct.cs" // `RuntimeHelpers.Box` of a struct holding a reference — the one shape whose box CoreCLR fills with `Buffer.BulkMoveWithWriteBarrier` rather than `SpanHelpers.Memmove` (RuntimeType.BoxCache.cs:91). Nothing here is about `ReflectionInvocation_GetBoxInfo`, which serves this type fine: the guest never reaches `Box` at all. It dies one step earlier, in its own `Unsafe.As<WithRef, byte>(ref r)`, which is `Intrinsics.call` (Intrinsics.fs:1672) taking a byte view over a *local* — measured as "refusing byte view over value type containing object references in single-cell byref `<variable 0 …> as System.Byte`", from `validateByteAddressableCell` (IlMachineManagedByref.fs:595). That is the same "a reference-containing value type has no byte image" gap as `ReinterpretCellUnderAliasedAncestor.cs` and `BulkMoveAcrossOverlappedStructPadding.cs`, and it forecloses *every* route a guest has to a `ref byte` over such a struct, which is why the reference-free half of this coverage lives in the active `RuntimeHelpersBox.cs` instead. The file is not satisfiable the wrong way: it checks both fields (so moving only the reference, or only the trailing int, still fails) and re-reads the box after mutating the source (so handing back a view onto the source rather than a copy fails). Un-park when a reference-containing value type can be byte-addressed.
            "DelegateBindOpenGenericDefinitionFormalSignature.cs" // `CreateDelegate` over a method of an open generic definition, where deciding compatibility needs a comparison against a type naming the definition's own variables: a parameter or return spelled with `T`, or an instance target's receiver, which is the typical instantiation `G<T>`. CoreCLR reads the signature against that typical instantiation and compares such a type as a `TypeVarTypeDesc` under its constraints (`IsLocationAssignable`, comdelegate.cpp:2367-2489), and the answer is not simply "incompatible": `T : class` makes a `T` return assignable to `object`, a nested enum `G<T>.E` matches `int` by the enum rule, and a contravariant `IContra<in T>`'s closed implementation passes as the receiver and binds to a working delegate. `NativeDelegate.isCompatible` refuses each such comparison at the point CoreCLR would make it, since every type it compares is a `ConcreteTypeHandle`. Measured on the first check: "TODO: Delegate_BindToMethodInfo must compare the target's first argument, <type param 0>, which names a type variable of the open generic definition declaring the target". Each of the eleven checks was measured separately to reach that refusal rather than a wrong answer. Un-park when delegate compatibility can compare a type in a definition's formal context. `IlMachineRuntimeMetadata.isRuntimeTypeHandleTargetAssignableTo` answers `CanCastTo` over such types for constraint validation, but `isCompatible` does not yet read the target's signature as `RuntimeTypeHandleTarget`s it could hand that oracle, and the enum and primitive rules of `IsLocationAssignable` are more than a cast. `DelegateBindOpenGenericDefinitionMethod.cs` covers the cases that never make such a comparison. Verified to exit 0 on real .NET.
            "DelegateBindOpenGenericDefinitionStaticVirtual.cs" // `CreateDelegate` over a static virtual method of an open generic interface definition, `typeof(I<>).GetMethod("M")`. Unlike every other method of an open definition, real .NET *binds* it: `BindToMethod` sends a virtual target on a non-value type to a virtual call stub over the typical instantiation (comdelegate.cpp:1237-1244), or virtualises it when closed over an object, and neither path asks for a code address, so no `InvalidOperationException`; the delegate raises `EntryPointNotFoundException` when invoked. PawPrint cannot build that stub, because `FunctionPointerTarget.VirtualCallStub` names a closed declaring type. Measured: "TODO: Delegate_BindToMethodInfo was asked to bind Abstract, a static virtual method of the open generic definition". Un-park when a virtual call stub can be minted over a definition's typical instantiation. Verified to exit 0 on real .NET.
            "PointerFieldAliasedWidthStore.cs" // Storing into a pointer-typed field through a byref aliased as `long*`/`double*` rather than `void**`. A pointer slot is a `CliType.RuntimePointer` cell with no byte image, so the byte-scatter writer refuses it; the sibling `PointerFieldIndirectStore.cs` fixes that for the pointer-shaped payloads by replacing the whole cell instead. That route is deliberately *not* taken here, because whole-cell replacement restamps the cell with the payload's shape: on a 64-bit runtime `stind.i8`/`stind.r8` are exact-width stores into a `void*` slot too, and taking them would leave the field holding `Numeric Int64`/`Float64`, so the next read pushes the wrong evaluation-stack kind and fails downstream (measured: `bad ceq: Int64 vs NativeInt(0)`) with a message naming neither the field nor the store. Un-parking needs a pointer cell that can hold a non-pointer bit pattern while still reading back as a pointer — i.e. the same "materialise bits late" question as the rest of the provenance model — not a wider routing predicate.
            "StackTraceFromExceptionNeedFileInfo.cs" // `new StackTrace(exception, fNeedFileInfo: true)` on an exception with no captured trace. The blocker is not the frame count, which is zero and correctly reported: `InitializeSourceInfo` calls `CreateStackTraceSymbols()` *before* the loop over frames, gated only on `fNeedFileInfo` (StackFrameHelper.cs:95-113), so an empty capture does not avoid it. Measured: "TODO: dispatch [UnsafeAccessor] is unimplemented for System.Diagnostics.StackFrameHelper::CreateStackTraceSymbols (kind=Constructor)". CoreLib wraps that block in `try { } catch { }`, which is how real .NET copes when `System.Diagnostics.StackTrace.dll` is absent, but that swallows a *guest* exception and a host-level refusal is not one. Un-park with `[UnsafeAccessor]` dispatch, or more cheaply by making an unresolvable `[UnsafeAccessor]` raise a guest exception, which CoreLib's own catch then absorbs. This is the blocker standing between `Exception.StackTrace` (Exception.cs:232) and `ExceptionDispatchInfo.SetCurrentStackTrace` (Exception.cs:247) and working, both of which pass `fNeedFileInfo: true`. Verified to exit 0 on real .NET.
        ]
        |> Set.ofList

    let expectsUnhandledException =
        [
            "UnhandledException.cs"
            "EnumHasFlagMismatchUnhandled.cs"
            "EnumHasFlagNullFlagUnhandled.cs"
            "ArrayGetLengthOutOfRangeUnhandled.cs"
            "CachedCctorFailureUnhandledOnFieldStore.cs"
            "CachedCctorFailureUnhandledOnCall.cs"
            "CachedCctorFailureUnhandledOnRunClassConstructor.cs"
        ]
        |> Set.ofList

    let customExitCodes =
        [
            "ExceptionWithNoOpFinally.cs", 3
            "ForegroundThreadExitsAfterMainReturns.cs", 7
            "IntMainReturnOverridesExitCode.cs", 3
            "VoidMainSetsExitCode.cs", 9
            "ExitOverridesExitCode.cs", 2
            "ForegroundWorkerTurnsBackgroundAfterMainReturns.cs", 5
            "BackgroundWorkerJoinsMainThread.cs", 4
            "EntryThreadIsBackgroundAfterMainReturns.cs", 11
            "ThreadStateOfEntryThreadAfterMain.cs", 11
            "EntryThreadReforegroundedThenBackgroundAgain.cs", 3
            "MainBackgroundBeforeStartingForegroundWorker.cs", 3
            "MainBackgroundThenForegroundAgainBeforeWorker.cs", 3
        ]
        |> Map.ofList

    let allPure =
        assy.GetManifestResourceNames ()
        |> Seq.choose (fun res ->
            let s = "WoofWare.PawPrint.Test.sourcesPure."

            if res.StartsWith (s, StringComparison.OrdinalIgnoreCase) then
                res.Substring s.Length |> Some
            else
                None
        )
        |> Set.ofSeq

    /// Guests that need a filesystem to look at, with the seed each one wants.
    ///
    /// The *same* seed configures both sides of the differential comparison:
    /// PawPrint realises it into a `VirtualFileSystem` rooted at `/`, and
    /// `RealRuntime.executeWithSeed` materialises it into the scratch directory
    /// the real guest runs in. One description, two interpreters — which is
    /// what makes the agreement worth something, rather than two hand-kept
    /// copies of a tree that might have drifted apart.
    ///
    /// Excluded from `simpleCases` because they need a non-default
    /// `KernelConfig`.
    let seededCases : Map<string, Map<DirectoryEntryName, SeedEntry>> =
        let name (s : string) =
            DirectoryEntryName.parseOrFail "test seed" s

        let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

        let file (contents : string) =
            SeedEntry.file (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

        let bytes (contents : string) =
            Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange

        let mode (raw : int) =
            PermissionBits.parseOrFail "test seed" raw

        let openSeed =
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                ]

        [
            "FileMetadataSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    // A leading dot, which is the whole of what "hidden" means
                    // on Unix.
                    name ".hidden", file "x"
                ]
            // Every mode here is written as an octal literal rather than
            // assembled from named bits, because the guest asserts the named
            // bits: if both sides were spelled the same way, a wrong bit order
            // would agree with itself.
            "FileModeSeeded.cs",
            Map.ofList
                [
                    name "default", file "hello"
                    name "private", SeedEntry.File (bytes "hello", mode 0o600)
                    name "shared", SeedEntry.File (bytes "hello", mode 0o666)
                    name "readonly", SeedEntry.File (bytes "hello", mode 0o444)
                    name "dir", SeedEntry.directory Map.empty
                    name "narrow", SeedEntry.Directory (Map.empty, mode 0o711)
                    // Non-empty *and* not writable by its owner, which is the
                    // one shape that makes the oracle's materialisation order
                    // observable: the child has to be created before the mode
                    // is applied, or the host cannot create it at all. (A
                    // process running as root bypasses that check, so on a root
                    // CI runner this seed exercises the order without being
                    // able to falsify it — the test still passes either way.)
                    name "locked", SeedEntry.Directory (Map.ofList [ name "inside", file "within" ], mode 0o555)
                ]
            "FileExistsSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "SystemNativeReadLink.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    name "lf", SeedEntry.Symlink (target "f")
                    // Six bytes, so that "exactly the target", "one byte more"
                    // and "one byte less" are three different buffer sizes.
                    // Dangling on purpose: `readlink` reports a target without
                    // resolving it, and a target that existed would let a
                    // handler that answered from the *resolved* file pass.
                    name "five", SeedEntry.Symlink (target "hello5")
                ]
            // Both open-path guests want the same tree, and deliberately share
            // one: the raw guest pins the syscall contract and the managed one
            // pins which exception each errno becomes, so a divergence between
            // them is a divergence about one filesystem rather than two.
            "UnlinkSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "g", file "bye"
                    name "held", file "payload"
                    name "f2", file "two"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lg", SeedEntry.Symlink (target "g")
                    name "dang", SeedEntry.Symlink (target "nx")
                ]
            "ChDirSeeded.cs",
            Map.ofList
                [
                    name "f", file "top"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "inside" ])
                    // Entering this must leave the process in `d`, not in `ld`.
                    name "ld", SeedEntry.Symlink (target "d")
                ]
            "RmDirSeeded.cs",
            Map.ofList
                [
                    name "empty", SeedEntry.directory Map.empty
                    name "full", SeedEntry.directory (Map.ofList [ name "x", file "inside" ])
                    name "f", file "hello"
                    // To a directory, so that following it would destroy `full`
                    // rather than merely answering the wrong errno.
                    name "ld", SeedEntry.Symlink (target "full")
                    name "dang", SeedEntry.Symlink (target "nx")
                    // Two levels, so that "nav/kid/." and "nav/kid/.." reach a
                    // directory that is not the root -- the flavours agree there
                    // and diverge at the root itself.
                    name "nav", SeedEntry.directory (Map.ofList [ name "kid", SeedEntry.directory Map.empty ])
                    // Opened before it is removed, which is the row that shows a
                    // descriptor outliving the last name.
                    name "held", SeedEntry.directory Map.empty
                ]
            "SystemNativeOpen.cs", openSeed
            "OpenMissingFile.cs", openSeed
            "LinkTargetSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    // A link to a link, so that following to the final target
                    // has to iterate rather than merely dereference once.
                    name "l2", SeedEntry.Symlink (target "lf")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                    // Longer than the 256-byte stackalloc `Interop.Sys.ReadLink`
                    // starts with, so reading it at all requires the truncating
                    // first call and the grown retry. NAME_MAX does not apply:
                    // this is a link's *target*, not anything's name.
                    name "long", SeedEntry.Symlink (target (String.replicate 300 "a"))
                ]
            "ReadAllBytesSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "empty", file ""
                    // U+00DF then 'x': three UTF-8 bytes for two characters, so
                    // a handler measuring .NET chars rather than bytes differs.
                    name "mb", file "\u00dfx"
                    name "lines", file "one\ntwo\nthree\n"
                    // 10000 bytes, longer than `StreamReader`'s 4096-byte
                    // buffer, so reading it issues several `pread`s at
                    // increasing offsets. The 251-byte cycle is coprime to 4096,
                    // so no chunk boundary lands on a repeat of the previous
                    // one's phase and an off-by-one shifts visible bytes.
                    name "big", file (String.init 10000 (fun i -> string<char> (char (int 'a' + (i % 251) % 26))))
                ]
            "FileStreamHandleSeeded.cs", Map.ofList [ name "f", file "hello" ]
            "CreateSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    name "lf", SeedEntry.Symlink (target "f")
                    // The link the plain-O_CREAT row follows: creating through it
                    // must bind "viadangtarget", not replace the link.
                    name "viadang", SeedEntry.Symlink (target "viadangtarget")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "PosixFAdviseSeeded.cs",
            Map.ofList
                [
                    // One file to read back through each hinted open, and a
                    // second to write through, so the write cannot disturb what
                    // the read rows expect.
                    name "f", file "hello"
                    name "g", file "hello"
                ]
            "TruncateSeeded.cs",
            Map.ofList
                [
                    // A separate file per group of rows, so each starts from a
                    // known five bytes rather than from whatever the previous
                    // group left behind.
                    name "f", file "hello"
                    name "g", file "hello"
                    name "h", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    // The symlink O_TRUNC follows: its *target* is what must end
                    // up empty, and the two names are distinct so that following
                    // it can be told from replacing it.
                    name "lf", SeedEntry.Symlink (target "f2")
                    name "f2", file "hello"
                    // The pair the refusal rows read back: a refused open must
                    // leave "keep" exactly as it was, whether it was refused for
                    // EEXIST or (through the link, under O_NOFOLLOW) for ELOOP.
                    name "keep", file "hello"
                    name "lkeep", SeedEntry.Symlink (target "keep")
                    // All four BCL rows need a *non-empty* file to start from:
                    // `SafeFileHandle.Init` swallows EINVAL and EBADF from
                    // FTruncate, so a wrongly-refused truncation shows up only as
                    // bytes that are still there.
                    name "bcl", file "hello"
                    name "bcl2", file "hello"
                    name "bcl3", file "hello"
                    name "bcl4", file "x"
                ]
            "WriteSeeded.cs",
            Map.ofList
                [
                    // Three separate files of the same contents, so that the
                    // syscall-level rows, the O_RDWR rows and the `FileStream`
                    // rows each start from a known five bytes rather than from
                    // whatever an earlier row left behind.
                    name "f", file "hello"
                    name "g", file "hello"
                    name "h", file "hello"
                    name "d", SeedEntry.directory Map.empty
                ]
            "ReadSeekSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    // A directory, because `read` and `lseek` disagree about
                    // one: reading it is EISDIR while seeking it (SEEK_SET and
                    // SEEK_CUR, the portable pair) succeeds.
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                ]
            "EnumerateSeeded.cs",
            Map.ofList
                [
                    name "d",
                    SeedEntry.directory (
                        Map.ofList
                            [
                                name "a", file "aaa"
                                name "sub", SeedEntry.directory (Map.ofList [ name "z", file "zzz" ])
                                name "ls", SeedEntry.Symlink (target "a")
                            ]
                    )
                    name "f", file "hello"
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "gone", SeedEntry.directory Map.empty
                    name "del",
                    SeedEntry.directory (
                        Map.ofList
                            [
                                name "x", file "xxx"
                                name "inner", SeedEntry.directory (Map.ofList [ name "y", file "yyy" ])
                            ]
                    )
                ]
            "MkDirSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    // A dangling link and a cyclic one: `mkdir` answers EEXIST
                    // for both, which is what says it never dereferences the
                    // name it is about to bind.
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "FlockContentionSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    // A second, unrelated file: a lock is per file, so holding
                    // one on `f` must not stop `g` being opened exclusively.
                    name "g", file "other"
                    // Another path to `f`. Locks are keyed on the resolved
                    // inode, not on the path used to reach it.
                    name "lf", SeedEntry.Symlink (target "f")
                ]
        ]
        |> Map.ofList

    let seededCaseNames : string list = seededCases |> Map.toList |> List.map fst

    /// Guests that need a particular environment variable, with the environment
    /// entries each one wants.
    ///
    /// As with `seededCases`, one description drives both sides: PawPrint's
    /// kernel holds the entries after its defaults, and `RealRuntime.executeWithSeed`
    /// overlays the variables they set on the environment the oracle process
    /// inherits from the test host. A guest here may assert only about the
    /// variables its entries name; the rest of the two environments have nothing
    /// to do with each other.
    let environmentCases : Map<string, string list> =
        [
            // 100 two-byte characters: 100 UTF-16 code units, 200 UTF-8 bytes,
            // which is the gap between the two units the guest exists to see.
            "EnvironmentVariableUtf8RequiredSize.cs",
            [
                EnvironmentPal.nameValueEntry "PAWPRINT_WIDE_VALUE" (System.String ('\u00e9', 100))
            ]
            // A name that is U+FFFD, which a lookup name holding an unpaired
            // surrogate finds.
            "EnvironmentVariableUnpairedSurrogateName.cs", [ "\uFFFD=found" ]
        ]
        |> Map.ofList

    let environmentCaseNames : string list =
        environmentCases |> Map.toList |> List.map fst

    let simpleCases : string list =
        allPure
        |> Seq.filter (fun s ->
            (customExitCodes.ContainsKey s
             || unimplemented.Contains s
             || expectsUnhandledException.Contains s
             || seededCases.ContainsKey s
             || environmentCases.ContainsKey s)
            |> not
        )
        |> Seq.toList

    /// Run `body` against a logger that captures the guest's run, writing everything it
    /// captured to stderr if `body` throws. The assertion belongs inside `body` rather
    /// than after it: a guest whose two runtimes disagree needs the same account of what
    /// PawPrint did as a guest that failed outright.
    let private withGuestLog (sourceName : string) (body : ILoggerFactory -> 'a) : 'a =
        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        try
            body loggerFactory
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let private interpret
        (loggerFactory : ILoggerFactory)
        (sourceName : string)
        (kernelConfig : KernelConfig)
        (image : byte array)
        : RunOutcome
        =
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        BoundedRun.run
            loggerFactory
            sourceName
            (Some sourceName)
            peImage
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel = kernelConfig
                    }
            }

    let runPawPrintSource
        (sourceName : string)
        (source : string)
        (kernelConfig : KernelConfig)
        (assertResult : byte array -> RunOutcome -> unit)
        : unit
        =
        let image = Roslyn.compile [ source ]

        withGuestLog
            sourceName
            (fun loggerFactory -> interpret loggerFactory sourceName kernelConfig image |> assertResult image)

    /// As `runPawPrintSource`, but the image also runs under the real runtime, and the two
    /// runs happen *at the same time* rather than one after the other. See
    /// `DifferentialOracle.alongsideInterpreted` for why overlapping them is sound. It is
    /// worth doing because the oracle is the larger half of an ordinary case's cost, and
    /// nearly all of that is spent blocked on the child process rather than computing.
    let runPawPrintSourceAgainstOracle
        (sourceName : string)
        (source : string)
        (kernelConfig : KernelConfig)
        (oracle : byte array -> RealRuntimeResult)
        (assertResult : RealRuntimeResult -> RunOutcome -> unit)
        : unit
        =
        let image = Roslyn.compile [ source ]

        withGuestLog
            sourceName
            (fun loggerFactory ->
                let realResult, pawPrintResult =
                    DifferentialOracle.alongsideInterpreted
                        (fun () -> oracle image)
                        (fun () -> interpret loggerFactory sourceName kernelConfig image)

                assertResult realResult pawPrintResult
            )

    let runTest (case : EndToEndTestCase) : unit =
        // Every `sourcesPure` case is `Always`: the directory's whole premise is that
        // the guest's claims hold on any host PawPrint's oracle can run on. A case that
        // needs a narrower policy belongs in `sourcesImpure`, where the policy is
        // declared per case.
        if case.Oracle <> OraclePolicy.Always then
            failwith
                $"%s{case.FileName} is registered as a *pure* differential case but declares Oracle = %O{case.Oracle}. A case whose claims hold on only one kernel belongs in sourcesImpure, where the policy is per case."

        DifferentialOracle.assertComparable case

        let source = Assembly.getEmbeddedResourceAsString case.FileName assy

        runPawPrintSourceAgainstOracle
            case.FileName
            source
            case.KernelConfig
            // The case's own seed and environment overlay drive the oracle
            // too, so both runtimes are looking at one description of a
            // filesystem and of the variables the case names. An unseeded
            // case passes `FileSystemSeed.empty` and an empty overlay, which
            // materialise nothing and leave the oracle exactly as it was.
            (RealRuntime.executeWithSeed case.KernelConfig.FileSystem case.KernelConfig.Environment [||])
            (fun realResult pawPrintResult ->
                DifferentialOracle.compareOutcomes
                    case.FileName
                    case.ExpectedReturnCode
                    case.ExpectsUnhandledException
                    realResult
                    pawPrintResult
            )

    /// `calli` through a null function pointer. This cannot be a comparison test in
    /// `sourcesPure`: the real runtime does not raise a catchable NullReferenceException
    /// here, it segfaults (observed as exit 139 on osx-arm64), which would take the test
    /// host down with it. A null function pointer is not correct CIL and ECMA-335 III.3.20
    /// does not say what happens to it, so PawPrint picks the deterministic catchable
    /// answer; see docs/divergences.md for why that one.
    [<Test>]
    let ``calli through a null function pointer throws NullReferenceException`` () =
        let source =
            """
using System;

public class Program
{
    public static unsafe int Main(string[] args)
    {
        // Two spellings of a null function pointer. Both currently reach the interpreter
        // as a verbatim zero, so this does not by itself exercise the other zero-valued
        // `NativeIntSource` shapes that `executeCalli` accepts as null (notably
        // `ManagedPointer ManagedPointerSource.Null`); that handling is deliberately
        // broader than any C# spelling reachable today. (`IntPtr.Zero.ToPointer()` would
        // be a third spelling, but it needs `ldsflda` of a MemberReference, which is a
        // separate unimplemented gap.)
        delegate*<int, int> a = null;
        delegate*<int, int> b = (delegate*<int, int>)(void*)null;

        int caught = 0;
        try { a(1); } catch (NullReferenceException) { caught += 1; }
        try { b(1); } catch (NullReferenceException) { caught += 2; }

        return caught == 3 ? 0 : caught;
    }
}
"""

        runPawPrintSource
            "CalliNullFunctionPointer.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, _) -> terminalState.LatchedExitCode |> shouldEqual 0
                | outcome ->
                    failwith
                        $"Expected the guest to catch a NullReferenceException from the null calli, got %O{outcome}"
            )

    /// ECMA-335 III.3.20 defines `calli`'s marshalling by the call-site StandaloneSignature,
    /// so a guest may legally pun a function pointer to a signature whose types differ from
    /// the target's. PawPrint invokes the target directly, so it is the *target's* types that
    /// drive argument coercion and the return push; until that is fixed (i.e. until arguments
    /// and the result are coerced to the call-site types) such a call must be refused at the
    /// faulting instruction rather than proceeding and failing far away inside
    /// `toCliTypeCoerced` with a message that never mentions `calli`.
    ///
    /// These cannot be `sourcesPure` comparison tests: CoreCLR accepts both (verified
    /// standalone on osx-arm64 — the return case prints 3, the argument case prints 7), so a
    /// comparison test would assert PawPrint reproduces behaviour it deliberately does not.
    /// See docs/divergences.md.
    [<Test>]
    let ``calli refuses a punned return type at the faulting instruction`` () =
        let source =
            """
using System;

public class Program
{
    static int Id(int x) => x;

    public static unsafe int Main(string[] args)
    {
        delegate*<int, int> p = &Id;
        // Same arity, same void-ness, wider return: passes the slot-count and void-ness
        // checks, and would otherwise die in toCliTypeCoerced at the `stloc` of the long.
        long r = ((delegate*<int, long>)p)(3);
        return (int)r;
    }
}
"""

        // `Assert.Catch`, not `Assert.Throws`, here and in the two tests below: `Assert.Throws` is
        // an *exact* type constraint, and a failure raised while interpreting arrives as
        // `GuestFailureException` carrying the guest's position. These tests are about the
        // message, not the type, so the looser assertion is the one that says what they mean.
        let exn =
            Assert.Catch (fun () ->
                runPawPrintSource "CalliPunnedReturn.cs" source KernelConfig.Default (fun _image _result -> ())
            )

        exn.Message |> shouldContainText "calli"
        exn.Message |> shouldContainText "return"
        exn.Message |> shouldContainText "Program"

    [<Test>]
    let ``calli refuses a punned parameter type at the faulting instruction`` () =
        let source =
            """
using System;

public class Program
{
    static long Id(long x) => x;

    public static unsafe int Main(string[] args)
    {
        delegate*<long, long> p = &Id;
        // Arity and return type agree; only the parameter's stack representation differs.
        long r = ((delegate*<int, long>)p)(7);
        return (int)r;
    }
}
"""

        let exn =
            Assert.Catch (fun () ->
                runPawPrintSource "CalliPunnedParameter.cs" source KernelConfig.Default (fun _image _result -> ())
            )

        exn.Message |> shouldContainText "calli"
        exn.Message |> shouldContainText "parameter"
        exn.Message |> shouldContainText "Program"

    /// `float32` and `float64` are the same type (`F`) on the CLI evaluation stack, but a
    /// `calli` marshals across a method boundary, where their ABI footprints differ. Reading a
    /// `float32` return slot as `float64` yields garbage on CoreCLR rather than the target's
    /// value, so this pun must be refused like the integer ones — otherwise PawPrint invokes
    /// the target and silently returns the *plausible* answer where the real runtime returns
    /// nonsense, which is worse than crashing.
    ///
    /// Measured on osx-arm64 with a bitmask probe over five puns (short/byte/uint/float
    /// returns and a signedness-punned parameter): CoreCLR returned 23 and PawPrint 31,
    /// differing on the float bit alone. That is why only `Single`/`Double` are separated and
    /// the integer widths and signedness deliberately are not — conflating those matches the
    /// real runtime, and splitting them would reject calls that work.
    [<Test>]
    let ``calli refuses a punned float width at the faulting instruction`` () =
        let source =
            """
public class Program
{
    static float Id(float x) => x;

    public static unsafe int Main(string[] args)
    {
        delegate*<float, float> p = &Id;
        double r = ((delegate*<float, double>)p)(1.5f);
        return r == 1.5 ? 42 : 7;
    }
}
"""

        let exn =
            Assert.Catch (fun () ->
                runPawPrintSource "CalliPunnedFloatWidth.cs" source KernelConfig.Default (fun _image _result -> ())
            )

        exn.Message |> shouldContainText "calli"
        exn.Message |> shouldContainText "return"
        exn.Message |> shouldContainText "Program"

    [<Test>]
    let ``Unhandled rethrow preserves original throw stack frame`` () =
        let source =
            """
using System;

class StackTraceSentinelException : Exception
{
}

class Program
{
    static void Blow()
    {
        throw new StackTraceSentinelException();
    }

    static int Main(string[] args)
    {
        try
        {
            Blow();
        }
        catch
        {
            throw;
        }

        return 1;
    }
}
"""

        runPawPrintSource
            "RethrowStackTrace.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    match exn.StackTrace with
                    | firstFrame :: _ -> firstFrame.Method.Name |> shouldEqual "Blow"
                    | [] -> failwith "Expected an unhandled rethrow to keep the original throw stack frame"
                | outcome -> failwith $"Expected an unhandled rethrow, got %O{outcome}"
            )

    [<Test>]
    let ``Emulated environment exposes invariant globalization switch`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        return Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") == "1" ? 0 : 1;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentInvariantGlobalization.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, _) -> terminalState.LatchedExitCode |> shouldEqual 0
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Emulated environment returns configured variables and null for missing variables`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("PAWPRINT_TEST_VARIABLE") != "configured")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") != "1")
        {
            return 5;
        }

        string missing = Environment.GetEnvironmentVariable("PAWPRINT_MISSING_VARIABLE");

        if (missing == "configured")
        {
            return 2;
        }

        if (missing == "")
        {
            return 3;
        }

        if (missing != null)
        {
            return 4;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentConfiguredVariables.cs"
            source
            { KernelConfig.Default with
                Environment = [ "PAWPRINT_TEST_VARIABLE=configured" ]
            }
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, _) -> terminalState.LatchedExitCode |> shouldEqual 0
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``GetEnvironmentVariableW lookup is case-sensitive`` () =
        // CoreCLR's Unix PAL implements the `kernel32!GetEnvironmentVariableW`
        // import with exact name comparison (see pal/src/misc/environ.cpp
        // `FindEnvVarValue`), so the QCall shim must do exact-string lookup
        // against the kernel env map even though the *Windows* kernel32 entry
        // would be case-insensitive: PawPrint is baselined against the host
        // runtime, which is the Unix PAL on the hosts this repo runs on.
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("PaWpRiNt_MiXeD_CaSe_KeY") != "found")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("pawprint_mixed_case_key") != null)
        {
            return 2;
        }

        if (Environment.GetEnvironmentVariable("PAWPRINT_MIXED_CASE_KEY") != null)
        {
            return 3;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentCaseSensitiveLookup.cs"
            source
            { KernelConfig.Default with
                Environment = [ "PaWpRiNt_MiXeD_CaSe_KeY=found" ]
            }
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, _) -> terminalState.LatchedExitCode |> shouldEqual 0
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Emulated environment preserves missing variable last PInvoke error`` () =
        let source =
            """
using System;
using System.Runtime.InteropServices;

class Program
{
    static int Main(string[] args)
    {
        Marshal.SetLastPInvokeError(0);

        string missing = Environment.GetEnvironmentVariable("PAWPRINT_MISSING_VARIABLE");

        if (missing != null)
        {
            return 1;
        }

        return Marshal.GetLastPInvokeError() == 203 ? 0 : 2;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentMissingVariableLastPInvokeError.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, _) -> terminalState.LatchedExitCode |> shouldEqual 0
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Environment.FailFast aborts execution`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        Environment.FailFast("boom");
        return 0;
    }
}
"""

        runPawPrintSource
            "EnvironmentFailFast.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.Aborted (_, _, fatal) ->
                    fatal.Code |> shouldEqual FatalErrorCode.FailFast
                    fatal.Message |> shouldEqual (Some "boom")
                | RunOutcome.NormalExit _ -> failwith "expected FailFast, got normal exit"
                | RunOutcome.ProcessExit _ -> failwith "expected FailFast, got process exit"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected FailFast, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"expected FailFast, got guest unhandled exception: %O{exn.ExceptionObject}"
            )

    [<TestCaseSource(nameof simpleCases)>]
    let ``Standard tests`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            Oracle = OraclePolicy.Always
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof seededCaseNames)>]
    let ``Seeded filesystem tests`` (fileName : string) =
        // A seed naming a Unix mode has no Windows equivalent, so the oracle
        // cannot stand in for it and the case is skipped rather than failed.
        // Asked of `RealRuntime` rather than decided here, so that the skip and
        // the validator's refusal cannot disagree about which seeds qualify.
        if not (RealRuntime.canMaterialise seededCases.[fileName]) then
            Assert.Ignore $"%s{fileName}'s seed names Unix permission bits, which this host cannot give a real file."

        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    FileSystem = seededCases.[fileName]
                }
            AppContext = AppContextProperties.empty
            Oracle = OraclePolicy.Always
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof environmentCaseNames)>]
    let ``Environment overlay tests`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    Environment = environmentCases.[fileName]
                }
            AppContext = AppContextProperties.empty
            Oracle = OraclePolicy.Always
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof customExitCodes)>]
    let ``Custom exit code tests`` (KeyValue (fileName : string, exitCode : int)) =
        if unimplemented.Contains fileName then
            Assert.Inconclusive ()

        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            Oracle = OraclePolicy.Always
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof expectsUnhandledException)>]
    let ``Tests which throw unhandled exceptions`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0 // not checked; both runtimes are expected to throw
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            Oracle = OraclePolicy.Always
            ExpectsUnhandledException = true
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof unimplemented)>]
    let ``Unimplemented tests have correct real-runtime behaviour`` (fileName : string) =
        let source = Assembly.getEmbeddedResourceAsString fileName assy
        let image = Roslyn.compile [ source ]

        let expectedExitCode =
            customExitCodes |> Map.tryFind fileName |> Option.defaultValue 0

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual expectedExitCode
        | RealRuntimeResult.UnhandledException report ->
            failwith $"Real runtime terminated with an unhandled exception for %s{fileName}:\n%s{report}"
        | RealRuntimeResult.Aborted (_code, report) ->
            failwith $"Real runtime called Environment.FailFast for %s{fileName}:\n%s{report}"

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            Oracle = OraclePolicy.Always
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    /// The one thing every `runPawPrintSource` case below wants from its outcome:
    /// that the guest ran to completion and returned the exit code it promised.
    /// Its own assertions are the interesting part, so a wrong code is reported
    /// with the code the guest actually chose.
    let private expectExitCode (expected : int) (outcome : RunOutcome) : IlMachineState =
        match outcome with
        | RunOutcome.NormalExit (terminalState, _) ->
            terminalState.LatchedExitCode |> shouldEqual expected
            terminalState
        | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
        | RunOutcome.Aborted (_, _, fatal) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"

    /// The variables `Environment.GetEnvironmentVariables` is asserted against
    /// below, chosen so that no single mistake in the environment block satisfies
    /// them all: an empty value (which must stay present-and-empty rather than
    /// vanishing, since an entry with no `=` is one CoreLib discards), a value
    /// containing `=` (which must not be split), non-ASCII including an astral
    /// character (two UTF-16 code units, so a byte/code-unit confusion shows), and
    /// two names where one is a prefix of the other.
    let private environmentVariablesSeed : (string * string) list =
        [
            "PAWPRINT_EMPTY", ""
            "PAWPRINT_EQUALS", "a=b=c"
            "PAWPRINT_UNICODE", "\u00e9\u4e2d\U0001F436"
            "PAWPRINT_P", "1"
            "PAWPRINT_PP", "2"
        ]

    /// `environmentVariablesSeed` plus the count the guest should see, derived
    /// from the kernel that configuration builds rather than written down: the
    /// kernel's environment is whichever `EmulatedKernel.defaultEnvironment`
    /// entries the seed does not name, then the seed, then the count entry,
    /// which is itself one more variable.
    ///
    /// Derived rather than hardcoded because the count is the assertion that
    /// catches a dropped or duplicated entry, and a hand-maintained number would
    /// silently stop matching if the seed or the defaults changed.
    let private environmentVariablesConfig : string list =
        let withCount (count : int) : string list =
            (environmentVariablesSeed
             |> List.map (fun (name, value) -> EnvironmentPal.nameValueEntry name value))
            @ [ EnvironmentPal.nameValueEntry "PAWPRINT_EXPECTED_COUNT" (string<int> count) ]

        // The count entry's value does not change how many variables there are,
        // so a kernel built with any count reports the real one.
        let kernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    Environment = withCount 0
                }

        kernel.Environment
        |> List.map EnvironmentPal.entryName
        |> List.distinct
        |> List.length
        |> withCount

    [<Test>]
    let ``GetEnvironmentVariables reports exactly the emulated environment`` () =
        // Impure in spirit — the value asserted is PawPrint's own seeded table,
        // which the real runtime cannot be an oracle for, since it would report
        // whatever environment the test host was started with. The cross-runtime
        // half of the contract lives in sourcesPure/EnvironmentGetVariables.cs.
        //
        // The strongest check here is the round-trip: every variable the
        // environment *block* reported must agree with what
        // `Environment.GetEnvironmentVariable` answers, and that reads
        // `Kernel.Environment` directly through a different QCall. So a block
        // whose entries split at the wrong `=` produces a name the table does not
        // hold, and fails — without this test having to know the table itself.
        let source =
            """
using System;
using System.Collections;

class Program
{
    static int Main(string[] args)
    {
        IDictionary vars = Environment.GetEnvironmentVariables();

        string expectedCount = Environment.GetEnvironmentVariable("PAWPRINT_EXPECTED_COUNT");
        if (expectedCount == null) return 1;

        // Pins that no entry was dropped or duplicated, which a missing or
        // doubled block terminator would cause.
        if (vars.Count != int.Parse(expectedCount)) return 2;

        foreach (DictionaryEntry entry in vars)
        {
            string key = (string)entry.Key;
            string value = (string)entry.Value;

            // CoreLib discards an entry whose first '=' is not after the first
            // character, so a key that is empty or contains '=' cannot come out
            // of a well-formed block at all.
            if (key.Length == 0) return 3;
            if (key.IndexOf('=') >= 0) return 4;

            if (Environment.GetEnvironmentVariable(key) != value) return 5;
        }

        // Present-and-empty, not absent.
        if (!vars.Contains("PAWPRINT_EMPTY")) return 6;
        if ((string)vars["PAWPRINT_EMPTY"] != "") return 7;

        // Only the first '=' separates; the rest belongs to the value.
        if ((string)vars["PAWPRINT_EQUALS"] != "a=b=c") return 8;

        // Three characters, four UTF-16 code units.
        if ((string)vars["PAWPRINT_UNICODE"] != "\u00e9\u4e2d\U0001F436") return 9;

        // A name that is a prefix of another stays distinct from it.
        if ((string)vars["PAWPRINT_P"] != "1") return 10;
        if ((string)vars["PAWPRINT_PP"] != "2") return 11;

        // A name never configured is absent rather than empty.
        if (vars.Contains("PAWPRINT_NEVER_SET")) return 12;

        // A second call must build a fresh dictionary from a fresh block, so the
        // first call cannot have cached or consumed anything.
        IDictionary again = Environment.GetEnvironmentVariables();
        if (ReferenceEquals(again, vars)) return 13;
        if (again.Count != vars.Count) return 14;
        if ((string)again["PAWPRINT_EQUALS"] != "a=b=c") return 15;

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentGetVariables.cs"
            source
            { KernelConfig.Default with
                Environment = environmentVariablesConfig
            }
            (fun _image pawPrintResult -> expectExitCode 0 pawPrintResult |> ignore<IlMachineState>)

    [<Test>]
    let ``GetEnvironmentVariables releases every block it allocates`` () =
        // A leaked environment block is invisible to the guest — no double free,
        // no use-after-free, and the next call simply allocates another — so the
        // only way to see whether `FreeEnvironmentStringsW` really frees is to
        // count what the native heap still owns when the process ends.
        //
        // Asserted as "independent of how many blocks were taken" rather than
        // against a fixed number, because the interpreter's own startup may leave
        // native blocks of its own and this test should not have to know how
        // many. If the free were a no-op, the four-call run would end with three
        // more live blocks than the one-call run.
        let source =
            """
using System;
using System.Collections;

class Program
{
    static int Main(string[] args)
    {
        string raw = Environment.GetEnvironmentVariable("PAWPRINT_CALL_COUNT");
        if (raw == null) return 1;

        int calls = int.Parse(raw);
        int total = 0;

        for (int i = 0; i < calls; i++)
        {
            IDictionary vars = Environment.GetEnvironmentVariables();
            total += vars.Count;
        }

        // Every call saw the same table, so a torn or partly-freed block shows up
        // here rather than being averaged away.
        if (total % calls != 0) return 2;

        return 0;
    }
}
"""

        let liveBlocksAfter (calls : int) : int =
            let mutable live = -1

            runPawPrintSource
                "EmulatedEnvironmentBlockLifetime.cs"
                source
                { KernelConfig.Default with
                    Environment = [ $"PAWPRINT_CALL_COUNT=%d{calls}" ]
                }
                (fun _image pawPrintResult ->
                    let terminalState = expectExitCode 0 pawPrintResult
                    live <- NativeMemoryPool.liveBlockCount terminalState.Kernel.NativeMemoryPool
                )

            live

        let afterOne = liveBlocksAfter 1
        let afterFour = liveBlocksAfter 4

        afterFour |> shouldEqual afterOne
