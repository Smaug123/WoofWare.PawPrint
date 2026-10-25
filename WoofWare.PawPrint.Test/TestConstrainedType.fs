namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// `ConstrainedType.resolve`, CoreCLR's retarget of a `constrained.` call on an enum to the enum's
/// underlying primitive where the method called holds `Object.GetHashCode`'s slot number, checked
/// against the host: the rule against the host's own `MethodDesc::GetSlot` over the methods of the
/// types an enum or a primitive answers to, and the escape analysis's dispatch against what probes
/// making such calls do on the real runtime. So too `ConstrainedType.resolveClassMethodOnValueType`,
/// what CoreCLR resolves a class's method to on a value type, against the host's method tables, and
/// the analysis's dispatch of such calls of a method that cannot be overridden against probes on the
/// real runtime. All read the host's own CoreLib, the image the host has loaded.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConstrainedType =

    /// `RuntimeMethodHandle.GetSlot(IRuntimeMethodInfo)`: CoreCLR's `MethodDesc::GetSlot`.
    let private getSlot : MethodInfo =
        typeof<RuntimeMethodHandle>.GetMethods (BindingFlags.NonPublic ||| BindingFlags.Static)
        |> Array.filter (fun m ->
            m.Name = "GetSlot"
            && m.GetParameters().Length = 1
            && m.GetParameters().[0].ParameterType.Name = "IRuntimeMethodInfo"
        )
        |> Array.exactlyOne

    /// The slot the host's CoreCLR gives `m` in its declaring type's method table.
    let private hostSlot (m : MethodBase) : int =
        getSlot.Invoke ((null : obj), [| box m |]) |> unbox<int>

    let private hostGetHashCodeSlot : int =
        hostSlot (typeof<obj>.GetMethod ("GetHashCode", Type.EmptyTypes))

    let private primitiveOf (ty : Type) : PrimitiveType =
        if ty = typeof<bool> then
            PrimitiveType.Boolean
        elif ty = typeof<char> then
            PrimitiveType.Char
        elif ty = typeof<sbyte> then
            PrimitiveType.SByte
        elif ty = typeof<byte> then
            PrimitiveType.Byte
        elif ty = typeof<int16> then
            PrimitiveType.Int16
        elif ty = typeof<uint16> then
            PrimitiveType.UInt16
        elif ty = typeof<int> then
            PrimitiveType.Int32
        elif ty = typeof<uint32> then
            PrimitiveType.UInt32
        elif ty = typeof<int64> then
            PrimitiveType.Int64
        elif ty = typeof<uint64> then
            PrimitiveType.UInt64
        elif ty = typeof<nativeint> then
            PrimitiveType.IntPtr
        elif ty = typeof<unativeint> then
            PrimitiveType.UIntPtr
        else
            failwith $"%s{ty.FullName} is not a primitive an enum can have as its underlying type"

    let private primitives : Type list =
        [
            typeof<bool>
            typeof<char>
            typeof<sbyte>
            typeof<byte>
            typeof<int16>
            typeof<uint16>
            typeof<int>
            typeof<uint32>
            typeof<int64>
            typeof<uint64>
        ]

    /// Every method declared by `ty`'s definition, constructors aside.
    let private declaredMethods (ty : Type) : MethodInfo list =
        let definition =
            if ty.IsGenericType then
                ty.GetGenericTypeDefinition ()
            else
                ty

        definition.GetMethods (
            BindingFlags.DeclaredOnly
            ||| BindingFlags.Public
            ||| BindingFlags.NonPublic
            ||| BindingFlags.Instance
            ||| BindingFlags.Static
        )
        |> List.ofArray

    [<Test>]
    let ``a constrained call on an enum is retargeted exactly where the host gives the method GetHashCode's slot``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = Assembly.readFile loggerFactory HostImage.coreLibPath
        let hostCoreLib = typeof<obj>.Assembly
        let bct = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ]

        let mutable state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        let concretize (spelled : TypeDefn) : ConcreteTypeHandle =
            let next, handle =
                TypeSystemState.concretizeType
                    loggerFactory
                    HostImage.runtimeDirs
                    bct
                    state
                    corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    spelled

            let handle = BindingResult.value handle

            state <- next
            handle

        let definitionOf (host : Type) : TypeDefn =
            let handle = MetadataTokens.TypeDefinitionHandle host.MetadataToken

            TypeDefn.FromDefinition (
                corelib.TypeDefs.[handle].Identity,
                if host.IsValueType then
                    SignatureTypeKind.ValueType
                else
                    SignatureTypeKind.Class
            )

        let pawPrintMethod (host : MethodInfo) =
            corelib.Methods.[MetadataTokens.MethodDefinitionHandle host.MetadataToken]

        // Every non-generic enum CoreLib defines.
        let enums =
            hostCoreLib.GetTypes ()
            |> Array.filter (fun ty -> ty.IsEnum && not ty.ContainsGenericParameters)
            |> List.ofArray

        // One enum of each underlying type CoreLib has an enum of, which every method is asked of.
        let representatives =
            enums
            |> List.groupBy Enum.GetUnderlyingType
            |> List.map (fun (_, group) -> group |> List.minBy (fun ty -> ty.FullName))

        // The methods of the types a primitive, an enum or a boxed value answers to: interfaces whose
        // methods number from zero, and so collide with `GetHashCode`'s slot, and classes whose own
        // virtual includes it.
        let methods =
            [ typeof<obj> ; typeof<ValueType> ; typeof<Enum> ]
            @ (primitives |> List.collect (fun p -> List.ofArray (p.GetInterfaces ())))
            @ List.ofArray (typeof<Enum>.GetInterfaces ())
            |> List.distinctBy (fun ty ->
                if ty.IsGenericType then
                    ty.GetGenericTypeDefinition ()
                else
                    ty
            )
            |> List.collect declaredMethods

        // A few methods every enum is asked of: the one whose slot `getCallInfo` compares against,
        // one in that slot on an interface every enum implements, and one in neither.
        let everyEnumAsked =
            [
                typeof<obj>.GetMethod ("GetHashCode", Type.EmptyTypes)
                typeof<IConvertible>.GetMethod "ToSByte"
                typeof<IConvertible>.GetMethod "GetTypeCode"
            ]

        // Types that are not enums, whatever their method in that slot.
        let notEnums =
            [ typeof<Enum> ; typeof<int> ; typeof<string> ; typeof<Guid> ; typeof<obj> ]

        let failures = ResizeArray<string> ()
        let mutable retargeted = 0
        let mutable kept = 0

        let check (constrained : Type) (methodInfo : MethodInfo) : unit =
            let handle = concretize (definitionOf constrained)

            let expected =
                if constrained.IsEnum && hostSlot methodInfo = hostGetHashCodeSlot then
                    let underlying =
                        concretize (TypeDefn.PrimitiveType (primitiveOf (Enum.GetUnderlyingType constrained)))

                    // CoreCLR resolves a class's method on a value type by its slot
                    // (`TryResolveConstraintMethodApprox`), and every class holds
                    // `Object.GetHashCode` in that one.
                    let dispatches =
                        if methodInfo.DeclaringType.IsInterface then
                            RetargetedCall.Named
                        else
                            RetargetedCall.ObjectGetHashCode

                    ConstrainedType.EnumUnderlying (handle, underlying, dispatches)
                else
                    ConstrainedType.AsNamed handle

            let next, actual =
                ConstrainedType.resolve loggerFactory HostImage.runtimeDirs bct state handle (pawPrintMethod methodInfo)
                |> BindingResult.bound

            state <- next

            match expected with
            | ConstrainedType.EnumUnderlying _ -> retargeted <- retargeted + 1
            | _ -> kept <- kept + 1

            if actual <> expected then
                failures.Add
                    $"constrained. %s{constrained.FullName} of %s{methodInfo.DeclaringType.Name}::%s{methodInfo.Name} (host slot %d{hostSlot methodInfo}): expected %A{expected}, got %A{actual}"

        for constrained in representatives @ notEnums do
            for methodInfo in methods do
                check constrained methodInfo

        for constrained in enums do
            for methodInfo in everyEnumAsked do
                check constrained methodInfo

        if failures.Count > 0 then
            failures |> String.concat Environment.NewLine |> failwith

        // Both ways the rule decides were exercised, enough for the comparison to say something.
        retargeted |> shouldBeGreaterThan 100
        kept |> shouldBeGreaterThan 1000

    [<Test>]
    let ``a class's method on a value type resolves to the method the host's tables give that slot`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = Assembly.readFile loggerFactory HostImage.coreLibPath
        let hostCoreLib = typeof<obj>.Assembly
        let bct = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ]

        let mutable state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        let concretize (host : Type) : ConcreteTypeHandle =
            let next, handle =
                TypeSystemState.concretizeType
                    loggerFactory
                    HostImage.runtimeDirs
                    bct
                    state
                    corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (
                        corelib.TypeDefs.[MetadataTokens.TypeDefinitionHandle host.MetadataToken].Identity,
                        SignatureTypeKind.ValueType
                    ))

            let handle = BindingResult.value handle

            state <- next
            handle

        let pawPrintMethod (host : MethodInfo) =
            corelib.Methods.[MetadataTokens.MethodDefinitionHandle host.MetadataToken]

        let instanceFlags =
            BindingFlags.DeclaredOnly
            ||| BindingFlags.Public
            ||| BindingFlags.NonPublic
            ||| BindingFlags.Instance

        // Every non-generic value type CoreLib defines.
        let valueTypes =
            hostCoreLib.GetTypes ()
            |> Array.filter (fun ty -> ty.IsValueType && not ty.ContainsGenericParameters)
            |> Array.sortBy (fun ty -> ty.FullName)
            |> List.ofArray

        // The instance methods of the classes every value type derives from, and of `String`, a sealed
        // class with virtuals of its own; and some of value types', which a call may name on another.
        let methods =
            [
                typeof<obj>
                typeof<ValueType>
                typeof<Enum>
                typeof<string>
                typeof<int>
                typeof<Guid>
            ]
            |> List.collect (fun ty -> List.ofArray (ty.GetMethods instanceFlags))

        // What the host's tables give: a method that is not virtual is no slot's, and a value type's
        // own virtual is taken itself. Otherwise the slot means the same in both tables where its
        // root declaration is of a class the value type derives from, and then holds the value type's
        // override of that declaration, if it has one.
        let expected (valueType : Type) (called : MethodInfo) : ClassMethodOnValueType =
            if not called.IsVirtual then
                ClassMethodOnValueType.Boxes
            elif called.DeclaringType.IsValueType then
                ClassMethodOnValueType.Resolves (pawPrintMethod called)
            else
                let root = called.GetBaseDefinition ()

                let ancestors =
                    valueType
                    |> List.unfold (fun ty -> Option.ofObj ty |> Option.map (fun ty -> ty, ty.BaseType))

                if not (List.contains root.DeclaringType ancestors) then
                    ClassMethodOnValueType.Undecided
                else
                    match
                        valueType.GetMethods instanceFlags
                        |> Array.tryFind (fun m -> m.IsVirtual && m.GetBaseDefinition () = root)
                    with
                    | Some own -> ClassMethodOnValueType.Resolves (pawPrintMethod own)
                    | None -> ClassMethodOnValueType.Boxes

        let summary (resolved : ClassMethodOnValueType) : string =
            match resolved with
            | ClassMethodOnValueType.Resolves m -> $"Resolves %s{m.Name} %A{m.IdentityKey}"
            | ClassMethodOnValueType.Boxes -> "Boxes"
            | ClassMethodOnValueType.Undecided -> "Undecided"

        let failures = ResizeArray<string> ()
        let counts = Dictionary<string, int> ()

        for valueType in valueTypes do
            let handle = concretize valueType

            for called in methods do
                let expected = expected valueType called

                let next, actual =
                    ConstrainedType.resolveClassMethodOnValueType
                        loggerFactory
                        HostImage.runtimeDirs
                        bct
                        state
                        handle
                        (pawPrintMethod called)
                    |> BindingResult.bound

                state <- next

                let kind = (summary expected).Split(' ').[0]

                counts.[kind] <-
                    (match counts.TryGetValue kind with
                     | true, n -> n
                     | false, _ -> 0)
                    + 1

                if summary actual <> summary expected then
                    failures.Add
                        $"%s{valueType.FullName} with %s{called.DeclaringType.Name}::%s{called.Name}: expected %s{summary expected}, got %s{summary actual}"

        if failures.Count > 0 then
            failures |> Seq.truncate 50 |> String.concat Environment.NewLine |> failwith

        // Every way the rule decides was exercised, enough for the comparison to say something.
        for kind in [ "Resolves" ; "Boxes" ; "Undecided" ] do
            (match counts.TryGetValue kind with
             | true, n -> n
             | false, _ -> 0)
            |> shouldBeGreaterThan 100

    /// What running a probe on the real runtime did.
    [<RequireQualifiedAccess>]
    type private Ran =
        | Returned
        | Threw of exceptionType : Type

    /// One probe: a method of the fabricated assembly making one `constrained.` call or load on an
    /// enum of the underlying type `Underlying`.
    type private Probe =
        {
            Name : string
            Underlying : Type
            /// The method the probe's instruction names.
            Called : MethodInfo
            /// The method the underlying primitive runs for `Called`.
            PrimitiveRuns : MethodInfo
            /// The method a boxed enum runs for `Called`, where `Enum` implements it.
            EnumRuns : MethodInfo option
            /// Whether the instruction is `ldftn`, rather than a call.
            LoadsPointer : bool
            /// Whether a generic method makes the call on its type variable, instantiated at the
            /// enum, rather than the probe on the enum it names.
            Generic : bool
            /// Whether a retargeted call passes the enum's address unboxed to `PrimitiveRuns`, a
            /// method the primitive declares, which itself boxes nothing: the probe then reaches no
            /// `CastHelpers` method that boxes.
            PassesUnboxed : bool
        }

    /// An enum, in the fabricated assembly, of each underlying type.
    let private enumName (underlying : Type) : string = "Fixture.E_" + underlying.Name

    /// The fabricated assembly: an enum of each of `primitives`, and each of `probes` as a static
    /// method of `Fixture.Probes`, taking the probe's enum by value for a call, or nothing for an
    /// `ldftn`.
    let private emitProbes (probes : Probe list) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "ConstrainedEnums", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "ConstrainedEnums"

        let enums =
            primitives
            |> List.map (fun underlying ->
                let e = modul.DefineEnum (enumName underlying, TypeAttributes.Public, underlying)
                e.CreateTypeInfo () |> ignore<TypeInfo>
                underlying, (e :> Type)
            )
            |> dict

        let probesType =
            modul.DefineType (
                "Fixture.Probes",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        // `ldarga 0`, then the arguments `probe.Called` takes, each its type's zero, then the call
        // under `constrained. on`, discarding what it returns.
        let emitCall (il : Emit.ILGenerator) (on : Type) (probe : Probe) : unit =
            if not probe.Called.IsStatic then
                il.Emit (Emit.OpCodes.Ldarga_S, 0uy)

            for parameter in probe.Called.GetParameters () do
                let ty = parameter.ParameterType

                if ty.IsByRef then
                    il.Emit (Emit.OpCodes.Ldloca, il.DeclareLocal (ty.GetElementType ()))
                elif ty.IsValueType then
                    let local = il.DeclareLocal ty
                    il.Emit (Emit.OpCodes.Ldloca, local)
                    il.Emit (Emit.OpCodes.Initobj, ty)
                    il.Emit (Emit.OpCodes.Ldloc, local)
                else
                    il.Emit Emit.OpCodes.Ldnull

            il.Emit (Emit.OpCodes.Constrained, on)

            il.Emit (
                (if probe.Called.IsStatic then
                     Emit.OpCodes.Call
                 else
                     Emit.OpCodes.Callvirt),
                probe.Called
            )

            if probe.Called.ReturnType <> typeof<Void> then
                il.Emit Emit.OpCodes.Pop

        for probe in probes do
            let enumType : Type = enums.[probe.Underlying]

            let il =
                probesType
                    .DefineMethod(
                        probe.Name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<Void>,
                        (if probe.LoadsPointer then
                             Type.EmptyTypes
                         else
                             [| enumType |])
                    )
                    .GetILGenerator ()

            if probe.LoadsPointer then
                il.Emit (Emit.OpCodes.Constrained, enumType)
                il.Emit (Emit.OpCodes.Ldftn, probe.Called)
                il.Emit Emit.OpCodes.Pop
            elif probe.Generic then
                // The call is made by a generic method of `T`, instantiated at the enum.
                let generic =
                    probesType.DefineMethod (
                        probe.Name + "_Generic",
                        MethodAttributes.Public ||| MethodAttributes.Static
                    )

                let t = generic.DefineGenericParameters([| "T" |]).[0]
                generic.SetReturnType typeof<Void>
                generic.SetParameters [| t :> Type |]
                let genericIl = generic.GetILGenerator ()
                emitCall genericIl t probe
                genericIl.Emit Emit.OpCodes.Ret

                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit (Emit.OpCodes.Call, generic.MakeGenericMethod [| enumType |])
            else
                emitCall il enumType probe

            il.Emit Emit.OpCodes.Ret

        probesType.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// What each of `probes` in `image` does on the real runtime, given its enum's zero.
    let private onRealRuntime (image : byte[]) (probes : Probe list) : Map<string, Ran> =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("ConstrainedEnums", isCollectible = true)

        try
            let assembly = context.LoadFromStream (new MemoryStream (image))
            let probesType = assembly.GetType "Fixture.Probes"

            probes
            |> List.map (fun probe ->
                let arguments =
                    if probe.LoadsPointer then
                        Array.empty<obj>
                    else
                        [| Activator.CreateInstance (assembly.GetType (enumName probe.Underlying)) |]

                let ran =
                    try
                        probesType.GetMethod(probe.Name).Invoke ((null : obj), arguments) |> ignore<obj>
                        Ran.Returned
                    with :? TargetInvocationException as e ->
                        Ran.Threw (e.InnerException.GetType ())

                probe.Name, ran
            )
            |> Map.ofList
        finally
            context.Unload ()

    /// The architecture the host's JIT compiles for.
    let private hostTarget () : JitTarget =
        match Runtime.InteropServices.RuntimeInformation.ProcessArchitecture with
        | Runtime.InteropServices.Architecture.Arm64 -> JitTarget.Arm64
        | Runtime.InteropServices.Architecture.X64 -> JitTarget.X64
        | other -> failwith $"No JIT table describes the host's architecture, %O{other}"

    /// The exceptions with which dispatch reports that a `constrained.` call's type supplies no
    /// method: `EntryPointNotFoundException` where a boxed receiver's class does not implement the
    /// interface, and `AmbiguousImplementationException` where no type supplies a static virtual
    /// and nothing but its abstract declaration is left.
    let private dispatchFailures : Type list =
        [
            typeof<EntryPointNotFoundException>
            typeof<System.Runtime.AmbiguousImplementationException>
        ]

    [<Test>]
    let ``the escape analysis dispatches a constrained call on an enum as the real runtime does`` () : unit =
        // An interface a primitive implements and `Enum` does not: on the real runtime, a method of
        // it that is not retargeted fails to dispatch on the enum, so the retarget shows.
        let enumInterfaces = HashSet<Type> (typeof<Enum>.GetInterfaces ())

        let probes =
            [
                for underlying in primitives do
                    let suffix = underlying.Name

                    let interfaces =
                        underlying.GetInterfaces ()
                        // A method of an interface CoreLib keeps internal raises
                        // `MethodAccessException` from the probe, which the analysis assumes away.
                        |> Array.filter (fun i -> i.IsVisible && not (enumInterfaces.Contains i))
                        |> Array.sortBy (fun i -> i.FullName)

                    for iface in interfaces do
                        let map = underlying.GetInterfaceMap iface

                        for index in 0 .. map.InterfaceMethods.Length - 1 do
                            let called = map.InterfaceMethods.[index]

                            // An interface's explicit implementation of another interface's
                            // method is private, which a call from outside cannot access.
                            if called.IsPublic && not called.IsGenericMethodDefinition then
                                let name = $"%s{suffix}_%s{iface.Name}_%s{called.Name}_%d{called.MetadataToken}"

                                let probe =
                                    {
                                        Name = "Call_" + name
                                        Underlying = underlying
                                        Called = called
                                        PrimitiveRuns = map.TargetMethods.[index]
                                        EnumRuns = None
                                        PassesUnboxed = false
                                        LoadsPointer = false
                                        Generic = false
                                    }

                                yield probe

                                if called.IsStatic then
                                    yield
                                        { probe with
                                            Name = "Ftn_" + name
                                            LoadsPointer = true
                                        }

                    // The method `getCallInfo` compares against, and one in its slot on an
                    // interface every enum implements, whose retarget the runtime does not show,
                    // because the enum's method and the primitive's agree.
                    let convertible = underlying.GetInterfaceMap typeof<IConvertible>
                    let enumConvertible = typeof<Enum>.GetInterfaceMap typeof<IConvertible>

                    let primitiveRunsOf (m : MethodInfo) =
                        convertible.TargetMethods.[Array.IndexOf (convertible.InterfaceMethods, m)]

                    let enumRunsOf (m : MethodInfo) =
                        enumConvertible.TargetMethods.[Array.IndexOf (enumConvertible.InterfaceMethods, m)]

                    // The call may name `GetHashCode` on any class an enum derives from: CoreCLR
                    // resolves each by its slot on the primitive, which derives from none but
                    // `Object`.
                    // A type variable's box may need a `CastHelpers` method, which an enum's does
                    // not, so only a call on one shows whether the receiver is boxed.
                    for declaring in [ typeof<obj> ; typeof<ValueType> ; typeof<Enum> ] do
                        for generic in [ false ; true ] do
                            yield
                                {
                                    Name =
                                        $"Call_%s{suffix}_%s{declaring.Name}_GetHashCode"
                                        + (if generic then "_OnVariable" else "")
                                    Underlying = underlying
                                    Called = declaring.GetMethod ("GetHashCode", Type.EmptyTypes)
                                    PrimitiveRuns = underlying.GetMethod ("GetHashCode", Type.EmptyTypes)
                                    EnumRuns = Some (typeof<Enum>.GetMethod ("GetHashCode", Type.EmptyTypes))
                                    PassesUnboxed = true
                                    LoadsPointer = false
                                    Generic = generic
                                }

                    yield
                        {
                            Name = $"Call_%s{suffix}_ToSByte"
                            Underlying = underlying
                            Called = typeof<IConvertible>.GetMethod "ToSByte"
                            PrimitiveRuns = primitiveRunsOf (typeof<IConvertible>.GetMethod "ToSByte")
                            EnumRuns = Some (enumRunsOf (typeof<IConvertible>.GetMethod "ToSByte"))
                            PassesUnboxed = false
                            LoadsPointer = false
                            Generic = false
                        }

                    yield
                        {
                            Name = $"Call_%s{suffix}_GetTypeCode"
                            Underlying = underlying
                            Called = typeof<IConvertible>.GetMethod "GetTypeCode"
                            PrimitiveRuns = primitiveRunsOf (typeof<IConvertible>.GetMethod "GetTypeCode")
                            EnumRuns = Some (enumRunsOf (typeof<IConvertible>.GetMethod "GetTypeCode"))
                            PassesUnboxed = false
                            LoadsPointer = false
                            Generic = false
                        }
            ]

        let image = emitProbes probes
        let runtime = onRealRuntime image probes

        let retargets (probe : Probe) : bool =
            hostSlot probe.Called = hostGetHashCodeSlot

        let failures = ResizeArray<string> ()

        // The rule is what the real runtime does, wherever the runtime shows it: a method of an
        // interface `Enum` does not implement dispatches on the enum only if it is retargeted,
        // unless it is a static virtual with a body of its own, which runs either way.
        let mutable shown = 0

        for probe in probes do
            let shows =
                not (enumInterfaces.Contains probe.Called.DeclaringType)
                && probe.Called.DeclaringType <> typeof<obj>
                && (not probe.Called.IsStatic || probe.Called.IsAbstract)

            if shows then
                shown <- shown + 1

                let failedToDispatch =
                    match runtime.[probe.Name] with
                    | Ran.Returned -> false
                    | Ran.Threw ty -> List.contains ty dispatchFailures

                if failedToDispatch = retargets probe then
                    failures.Add
                        $"%s{probe.Name}: the runtime %A{runtime.[probe.Name]}, but the host gives it slot %d{hostSlot probe.Called} against GetHashCode's %d{hostGetHashCodeSlot}"

        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = Assembly.readFile loggerFactory HostImage.coreLibPath

        let fixture =
            Assembly.read loggerFactory (Some "ConstrainedEnums.dll") (new MemoryStream (image))

        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; fixture ]
        let bct = BaseClassTypes.ofCorelib corelib

        let mutable analysis =
            EscapeAnalysis.create
                loggerFactory
                HostImage.runtimeDirs
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                {
                    ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
                    LoadedAssemblies = loaded
                    BaseTypes = bct
                }

        let probeKey (probe : Probe) : MethodKey =
            fixture.Methods
            |> Seq.pick (fun (KeyValue (handle, method)) ->
                if method.Name = probe.Name then
                    Some (MethodKey.make fixture handle)
                else
                    None
            )

        let corelibKey (m : MethodInfo) : MethodKey =
            MethodKey.make corelib (MetadataTokens.MethodDefinitionHandle m.MetadataToken)

        let mutable retargeted = 0

        for probe in probes do
            let key = probeKey probe
            let next, escapes = EscapeAnalysis.escapes analysis key
            let next, sources = EscapeAnalysis.unknownSources next key
            analysis <- next

            let inProbe = sources |> Seq.filter (fun site -> site.Method = key) |> List.ofSeq

            let raised =
                escapes.Types
                |> Seq.map (fun thrown ->
                    match thrown with
                    | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
                    | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
                )
                |> Set.ofSeq

            let describe () =
                $"%s{probe.Name}: escapes %A{Set.toList raised}, unknown %b{escapes.Unknown}, opaque in the probe at %A{inProbe |> List.map (fun site -> site.Reason)}; the runtime %A{runtime.[probe.Name]}"

            // Everything the real runtime raised is reported.
            match runtime.[probe.Name] with
            | Ran.Threw ty when not escapes.Unknown ->
                let rec named (ty : Type) =
                    raised.Contains ("=" + ty.FullName)
                    || raised.Contains ("<:" + ty.FullName)
                    || (
                        match ty.BaseType with
                        | null -> false
                        | baseType -> raised.Contains ("<:" + baseType.FullName) || named baseType
                    )

                if not (named ty) then
                    failures.Add $"%s{describe ()}, which lacks %s{ty.FullName}"
            | _ -> ()

            if retargets probe then
                retargeted <- retargeted + 1

                // The call is resolved on the primitive, so it is decided in the probe, and a call
                // reaches the primitive's method.
                if not inProbe.IsEmpty then
                    failures.Add $"%s{describe ()}, but it is retargeted, which decides it"

                if not probe.LoadsPointer then
                    let next, reached = EscapeAnalysis.reachable analysis key
                    analysis <- next

                    if not (reached.Contains (corelibKey probe.PrimitiveRuns)) then
                        failures.Add
                            $"%s{describe ()}, but it is retargeted to %s{probe.PrimitiveRuns.DeclaringType.Name}::%s{probe.PrimitiveRuns.Name}, which it does not reach"

                    // `Enum`'s methods are not called, however they would reach the primitive's.
                    match probe.EnumRuns with
                    | Some enumRuns when reached.Contains (corelibKey enumRuns) ->
                        failures.Add $"%s{describe ()}, which reaches Enum::%s{enumRuns.Name}, though it is retargeted"
                    | _ -> ()

                    if probe.PassesUnboxed then
                        let boxes =
                            reached
                            |> Seq.map (EscapeAnalysis.methodName analysis)
                            |> Seq.filter (fun name ->
                                name.StartsWith "System.Runtime.CompilerServices.CastHelpers::Box"
                            )
                            |> List.ofSeq

                        if not boxes.IsEmpty then
                            failures.Add $"%s{describe ()}, which boxes its receiver by %A{boxes}"
            elif not probe.LoadsPointer then
                let next, reached = EscapeAnalysis.reachable analysis key
                analysis <- next

                match probe.EnumRuns with
                | Some enumRuns ->
                    if not (reached.Contains (corelibKey enumRuns)) then
                        failures.Add
                            $"%s{describe ()}, which does not reach Enum::%s{enumRuns.Name}, though it is not retargeted"
                | None ->
                    if reached.Contains (corelibKey probe.PrimitiveRuns) then
                        failures.Add
                            $"%s{describe ()}, which reaches %s{probe.PrimitiveRuns.DeclaringType.Name}::%s{probe.PrimitiveRuns.Name}, though it is not retargeted"

        if failures.Count > 0 then
            failures |> String.concat Environment.NewLine |> failwith

        shown |> shouldBeGreaterThan 500
        retargeted |> shouldBeGreaterThan 40

    /// A method a probe of the second fixture names, or that runs for it.
    [<RequireQualifiedAccess>]
    type private ProbeMethod =
        /// The method of this name, taking nothing, that this type of the fixture declares.
        | Fixture of declaring : string * name : string
        | CoreLib of MethodInfo

    /// What the real runtime runs for a `constrained.` call, on a value type, of a class's method
    /// that cannot be overridden.
    [<RequireQualifiedAccess>]
    type private Runs =
        /// This method, on the receiver's address: the value type's own in the slot of the method
        /// named, or, on an enum, its underlying primitive's in `GetHashCode`'s slot.
        | Own of ProbeMethod
        /// The method named, on a box of the receiver, which is an instance of the method's class.
        | NamedOnBox
        /// The method named, on a box of the receiver, which is no instance of the method's class.
        | NamedOnForeignBox

    /// What shows, on the real runtime, which method a probe of the second fixture runs.
    [<RequireQualifiedAccess>]
    type private Shows =
        /// The probe returns what that method returns, called on a zero of its declaring type, or on
        /// the receiver for a method of `Enum`. Each method of the fixture returns a constant of its
        /// own.
        | ItsReturn
        /// The probe returns the box of the receiver that the method was called on, as
        /// `String.ToString` returns `this`.
        | ReceiverBox
        /// Nothing does.
        | Nothing

    /// One probe of the second fixture: a method of `Fixture.Probes` taking the fixture's type `On`
    /// by value and returning, boxed, what `constrained. On callvirt Called` returns.
    type private NonOverridableProbe =
        {
            Name : string
            On : string
            Called : ProbeMethod
            /// Whether a generic method makes the call on its type variable, instantiated at `On`.
            Generic : bool
            Runs : Runs
            Shows : Shows
        }

    /// The second fixture: a sealed class `Fixture.Sealed`, overriding `ToString` and `GetHashCode`
    /// and declaring `NonVirt`; a class `Fixture.OpenFinal` with a final `ToString`; the structs
    /// `Fixture.Plain`, which overrides nothing, and `Fixture.Overriding`, which overrides `ToString`
    /// and `GetHashCode`; the enum `Fixture.E` over `int`; and each of `probes`.
    let private emitNonOverridableProbes (probes : NonOverridableProbe list) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "ConstrainedNonOverridable", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "ConstrainedNonOverridable"

        let returning
            (ty : Emit.TypeBuilder)
            (name : string)
            (attributes : MethodAttributes)
            (returns : Type)
            (emit : Emit.ILGenerator -> unit)
            : unit
            =
            let il =
                ty
                    .DefineMethod(
                        name,
                        MethodAttributes.Public ||| MethodAttributes.HideBySig ||| attributes,
                        returns,
                        Type.EmptyTypes
                    )
                    .GetILGenerator ()

            emit il
            il.Emit Emit.OpCodes.Ret

        let toString (ty : Emit.TypeBuilder) (attributes : MethodAttributes) (text : string) : unit =
            returning
                ty
                "ToString"
                (MethodAttributes.Virtual ||| attributes)
                typeof<string>
                (fun il -> il.Emit (Emit.OpCodes.Ldstr, text))

        let getHashCode (ty : Emit.TypeBuilder) (value : int) : unit =
            returning
                ty
                "GetHashCode"
                MethodAttributes.Virtual
                typeof<int>
                (fun il -> il.Emit (Emit.OpCodes.Ldc_I4, value))

        let sealedType =
            modul.DefineType ("Fixture.Sealed", TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<obj>)

        toString sealedType MethodAttributes.PrivateScope "Sealed.ToString"
        getHashCode sealedType 4242

        returning
            sealedType
            "NonVirt"
            MethodAttributes.PrivateScope
            typeof<string>
            (fun il -> il.Emit (Emit.OpCodes.Ldstr, "Sealed.NonVirt"))

        sealedType.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<Emit.ConstructorBuilder>

        let openFinal =
            modul.DefineType ("Fixture.OpenFinal", TypeAttributes.Public, typeof<obj>)

        toString openFinal MethodAttributes.Final "OpenFinal.ToString"

        openFinal.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<Emit.ConstructorBuilder>

        let defineStruct (name : string) (overrides : bool) : Type =
            let ty =
                modul.DefineType (
                    name,
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            ty.DefineField ("value", typeof<int64>, FieldAttributes.Public)
            |> ignore<Emit.FieldBuilder>

            if overrides then
                toString ty MethodAttributes.PrivateScope "Overriding.ToString"
                getHashCode ty 77

            ty.CreateType ()

        let types =
            [
                "Fixture.Sealed", sealedType.CreateType ()
                "Fixture.OpenFinal", openFinal.CreateType ()
                "Fixture.Plain", defineStruct "Fixture.Plain" false
                "Fixture.Overriding", defineStruct "Fixture.Overriding" true
                "Fixture.E", modul.DefineEnum("Fixture.E", TypeAttributes.Public, typeof<int>).CreateTypeInfo () :> Type
            ]
            |> dict

        let probesType =
            modul.DefineType (
                "Fixture.Probes",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        // `ldarga 0; constrained. on; callvirt called`, returning what the call returns, boxed.
        let emitCall (il : Emit.ILGenerator) (on : Type) (called : MethodInfo) : unit =
            il.Emit (Emit.OpCodes.Ldarga_S, 0uy)
            il.Emit (Emit.OpCodes.Constrained, on)
            il.Emit (Emit.OpCodes.Callvirt, called)

            if called.ReturnType.IsValueType then
                il.Emit (Emit.OpCodes.Box, called.ReturnType)

            il.Emit Emit.OpCodes.Ret

        for probe in probes do
            let on = types.[probe.On]

            let called =
                match probe.Called with
                | ProbeMethod.Fixture (declaring, name) -> types.[declaring].GetMethod (name, Type.EmptyTypes)
                | ProbeMethod.CoreLib called -> called

            let il =
                probesType
                    .DefineMethod(
                        probe.Name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<obj>,
                        [| on |]
                    )
                    .GetILGenerator ()

            if probe.Generic then
                let generic =
                    probesType.DefineMethod (
                        probe.Name + "_Generic",
                        MethodAttributes.Public ||| MethodAttributes.Static
                    )

                let t = generic.DefineGenericParameters([| "T" |]).[0]
                generic.SetReturnType typeof<obj>
                generic.SetParameters [| t :> Type |]
                emitCall (generic.GetILGenerator ()) t called

                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit (Emit.OpCodes.Call, generic.MakeGenericMethod [| on |])
                il.Emit Emit.OpCodes.Ret
            else
                emitCall il on called

        probesType.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``the escape analysis dispatches a constrained call of a class's method that cannot be overridden as the real runtime does``
        ()
        : unit
        =
        let stringToString = typeof<string>.GetMethod ("ToString", Type.EmptyTypes)
        let stringGetHashCode = typeof<string>.GetMethod ("GetHashCode", Type.EmptyTypes)
        let int32GetHashCode = typeof<int>.GetMethod ("GetHashCode", Type.EmptyTypes)
        let enumGetTypeCode = typeof<Enum>.GetMethod ("GetTypeCode", Type.EmptyTypes)

        // The CoreLib methods the probes name cannot be overridden, as the fixture's cannot.
        for m in [ stringToString ; stringGetHashCode ; enumGetTypeCode ] do
            (m.IsVirtual && (m.IsFinal || m.DeclaringType.IsSealed)) |> shouldEqual true

        let sealedMethod (name : string) =
            ProbeMethod.Fixture ("Fixture.Sealed", name)

        let openFinalToString = ProbeMethod.Fixture ("Fixture.OpenFinal", "ToString")

        let overriding (name : string) =
            Runs.Own (ProbeMethod.Fixture ("Fixture.Overriding", name))

        let primitiveGetHashCode = Runs.Own (ProbeMethod.CoreLib int32GetHashCode)

        // The type the prefix names, the method named, what runs, and what shows it.
        let calls =
            [
                "Fixture.Plain", sealedMethod "ToString", Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.Plain", sealedMethod "GetHashCode", Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.Plain", sealedMethod "NonVirt", Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.Plain", openFinalToString, Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.Plain", ProbeMethod.CoreLib stringToString, Runs.NamedOnForeignBox, Shows.ReceiverBox
                // `String.GetHashCode` hashes the box's bytes as though they were a string's.
                "Fixture.Plain", ProbeMethod.CoreLib stringGetHashCode, Runs.NamedOnForeignBox, Shows.Nothing
                "Fixture.Overriding", sealedMethod "ToString", overriding "ToString", Shows.ItsReturn
                "Fixture.Overriding", sealedMethod "GetHashCode", overriding "GetHashCode", Shows.ItsReturn
                "Fixture.Overriding", sealedMethod "NonVirt", Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.Overriding", openFinalToString, overriding "ToString", Shows.ItsReturn
                "Fixture.Overriding", ProbeMethod.CoreLib stringToString, overriding "ToString", Shows.ItsReturn
                "Fixture.Overriding", ProbeMethod.CoreLib stringGetHashCode, overriding "GetHashCode", Shows.ItsReturn
                "Fixture.E", sealedMethod "ToString", Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.E", sealedMethod "GetHashCode", primitiveGetHashCode, Shows.ItsReturn
                "Fixture.E", sealedMethod "NonVirt", Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.E", openFinalToString, Runs.NamedOnForeignBox, Shows.ItsReturn
                "Fixture.E", ProbeMethod.CoreLib stringToString, Runs.NamedOnForeignBox, Shows.ReceiverBox
                "Fixture.E", ProbeMethod.CoreLib stringGetHashCode, primitiveGetHashCode, Shows.ItsReturn
                "Fixture.E", ProbeMethod.CoreLib enumGetTypeCode, Runs.NamedOnBox, Shows.ItsReturn
            ]

        let probes =
            [
                for generic in [ false ; true ] do
                    for on, called, runs, shows in calls do
                        let calledName =
                            match called with
                            | ProbeMethod.Fixture (declaring, name) -> declaring + "_" + name
                            | ProbeMethod.CoreLib m -> m.DeclaringType.Name + "_" + m.Name

                        yield
                            {
                                Name =
                                    $"%s{on}_%s{calledName}".Replace ('.', '_')
                                    + (if generic then "_OnVariable" else "")
                                On = on
                                Called = called
                                Generic = generic
                                Runs = runs
                                Shows = shows
                            }
            ]

        let image = emitNonOverridableProbes probes
        let failures = ResizeArray<string> ()

        // Whether a method is the fixture's, and its token, as the analysis is to find it.
        let identify (fixtureAssembly : Assembly) (m : MethodInfo) : bool * int =
            m.DeclaringType.Assembly = fixtureAssembly, m.MetadataToken

        // On the real runtime, for each probe: the method the probe says runs, the method it names,
        // and what it returned, having checked that against what shows which method ran.
        let ranOn =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("ConstrainedNonOverridable", isCollectible = true)

            try
                let assembly = context.LoadFromStream (new MemoryStream (image))
                let probesType = assembly.GetType "Fixture.Probes"

                let resolve (m : ProbeMethod) : MethodInfo =
                    match m with
                    | ProbeMethod.Fixture (declaring, name) ->
                        assembly.GetType(declaring).GetMethod (name, Type.EmptyTypes)
                    | ProbeMethod.CoreLib m -> m

                probes
                |> List.map (fun probe ->
                    let named = resolve probe.Called

                    let runs =
                        match probe.Runs with
                        | Runs.Own own -> resolve own
                        | Runs.NamedOnBox
                        | Runs.NamedOnForeignBox -> named

                    let receiver = Activator.CreateInstance (assembly.GetType probe.On)

                    let returned =
                        try
                            Ok (probesType.GetMethod(probe.Name).Invoke ((null : obj), [| receiver |]))
                        with :? TargetInvocationException as e ->
                            Error (e.InnerException.GetType ())

                    let expected =
                        match probe.Shows with
                        | Shows.ItsReturn ->
                            let zero =
                                if runs.DeclaringType = typeof<Enum> then
                                    receiver
                                else
                                    Activator.CreateInstance runs.DeclaringType

                            Some (Ok (runs.Invoke (zero, [||])))
                        | Shows.ReceiverBox -> Some (Ok receiver)
                        | Shows.Nothing -> None

                    match expected with
                    | Some expected when expected <> returned ->
                        failures.Add
                            $"%s{probe.Name}: the real runtime returned %A{returned}, but %s{runs.DeclaringType.Name}::%s{runs.Name}, which the probe says runs, returns %A{expected}"
                    | _ -> ()

                    probe.Name, (identify assembly runs, identify assembly named, returned)
                )
                |> Map.ofList
            finally
                context.Unload ()

        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = Assembly.readFile loggerFactory HostImage.coreLibPath

        let fixture =
            Assembly.read loggerFactory (Some "ConstrainedNonOverridable.dll") (new MemoryStream (image))

        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; fixture ]
        let bct = BaseClassTypes.ofCorelib corelib

        let mutable analysis =
            EscapeAnalysis.create
                loggerFactory
                HostImage.runtimeDirs
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                {
                    ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
                    LoadedAssemblies = loaded
                    BaseTypes = bct
                }

        let fixtureKeyNamed (name : string) : MethodKey =
            fixture.Methods
            |> Seq.pick (fun (KeyValue (handle, method)) ->
                if method.Name = name then
                    Some (MethodKey.make fixture handle)
                else
                    None
            )

        let keyOf (inFixture : bool, token : int) : MethodKey =
            MethodKey.make (if inFixture then fixture else corelib) (MetadataTokens.MethodDefinitionHandle token)

        let mutable own = 0
        let mutable foreign = 0

        for probe in probes do
            let key = fixtureKeyNamed probe.Name

            // The methods that make the call: the probe, or its generic method's instance.
            let makers =
                Set.ofList
                    [
                        yield key
                        if probe.Generic then
                            yield fixtureKeyNamed (probe.Name + "_Generic")
                    ]

            let next, escapes = EscapeAnalysis.escapes analysis key
            let next, sources = EscapeAnalysis.unknownSources next key
            let next, reached = EscapeAnalysis.reachable next key
            analysis <- next

            let inProbe =
                sources |> Seq.filter (fun site -> makers.Contains site.Method) |> List.ofSeq

            let runs, named, returned = ranOn.[probe.Name]
            let runs = keyOf runs
            let named = keyOf named

            let raised =
                escapes.Types
                |> Seq.map (fun thrown ->
                    match thrown with
                    | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
                    | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
                )
                |> List.ofSeq

            let describe () =
                $"%s{probe.Name}: escapes %A{raised}, unknown %b{escapes.Unknown}, opaque in the probe at %A{inProbe |> List.map (fun site -> site.Reason)}; the real runtime ran %s{EscapeAnalysis.methodName analysis runs} and returned %A{returned}"

            if not (reached.Contains runs) then
                failures.Add $"%s{describe ()}, which the analysis does not reach"

            match probe.Runs with
            | Runs.Own _ ->
                own <- own + 1

                if not inProbe.IsEmpty then
                    failures.Add $"%s{describe ()}, but the value type's own method decides it"

                if reached.Contains named then
                    failures.Add
                        $"%s{describe ()}, which reaches %s{EscapeAnalysis.methodName analysis named}, though the value type's own method runs"

                let boxes =
                    reached
                    |> Seq.map (EscapeAnalysis.methodName analysis)
                    |> Seq.filter (fun name -> name.StartsWith "System.Runtime.CompilerServices.CastHelpers::Box")
                    |> List.ofSeq

                if not boxes.IsEmpty then
                    failures.Add $"%s{describe ()}, which boxes its receiver by %A{boxes}"
            | Runs.NamedOnBox ->
                if not inProbe.IsEmpty then
                    failures.Add $"%s{describe ()}, but the box is an instance of the method's class, which decides it"
            | Runs.NamedOnForeignBox ->
                foreign <- foreign + 1

                if not (inProbe |> List.exists (fun site -> site.Reason = Opacity.MistypedReceiver)) then
                    failures.Add
                        $"%s{describe ()}, which is not opaque for its receiver, though the method runs on a box that is no instance of its class"

        if failures.Count > 0 then
            failures |> String.concat Environment.NewLine |> failwith

        own |> shouldBeGreaterThan 10
        foreign |> shouldBeGreaterThan 10
