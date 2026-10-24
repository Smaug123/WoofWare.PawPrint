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
/// making such calls do on the real runtime. Both read the host's own CoreLib, the image the host
/// has loaded.
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

                    ConstrainedType.EnumUnderlying (handle, underlying)
                else
                    ConstrainedType.AsNamed handle

            let next, actual =
                ConstrainedType.resolve loggerFactory HostImage.runtimeDirs bct state handle (pawPrintMethod methodInfo)

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
            else
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

                il.Emit (Emit.OpCodes.Constrained, enumType)

                il.Emit (
                    (if probe.Called.IsStatic then
                         Emit.OpCodes.Call
                     else
                         Emit.OpCodes.Callvirt),
                    probe.Called
                )

                if probe.Called.ReturnType <> typeof<Void> then
                    il.Emit Emit.OpCodes.Pop

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
                        |> Array.filter (fun i -> not (enumInterfaces.Contains i))
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
                                        LoadsPointer = false
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

                    yield
                        {
                            Name = $"Call_%s{suffix}_GetHashCode"
                            Underlying = underlying
                            Called = typeof<obj>.GetMethod ("GetHashCode", Type.EmptyTypes)
                            PrimitiveRuns = underlying.GetMethod ("GetHashCode", Type.EmptyTypes)
                            EnumRuns = Some (typeof<Enum>.GetMethod ("GetHashCode", Type.EmptyTypes))
                            LoadsPointer = false
                        }

                    yield
                        {
                            Name = $"Call_%s{suffix}_ToSByte"
                            Underlying = underlying
                            Called = typeof<IConvertible>.GetMethod "ToSByte"
                            PrimitiveRuns = primitiveRunsOf (typeof<IConvertible>.GetMethod "ToSByte")
                            EnumRuns = Some (enumRunsOf (typeof<IConvertible>.GetMethod "ToSByte"))
                            LoadsPointer = false
                        }

                    yield
                        {
                            Name = $"Call_%s{suffix}_GetTypeCode"
                            Underlying = underlying
                            Called = typeof<IConvertible>.GetMethod "GetTypeCode"
                            PrimitiveRuns = primitiveRunsOf (typeof<IConvertible>.GetMethod "GetTypeCode")
                            EnumRuns = Some (enumRunsOf (typeof<IConvertible>.GetMethod "GetTypeCode"))
                            LoadsPointer = false
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
