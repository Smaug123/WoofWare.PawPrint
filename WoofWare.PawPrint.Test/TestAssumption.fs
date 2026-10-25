namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Globalization
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// An `Assumption`, the resource lookup's contract (`ResourceLookup`) and `Object.GetType`'s
/// (`ObjectGetType`) each stand in for a CoreLib method they recognise by name and signature. These
/// tests hold each to the CoreLibs: the method must be there, as the contract describes it
/// (`TestObjectGetType` holds the last).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssumption =

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    [<TestCaseSource(nameof coreLibs)>]
    let ``every assumption summarises the methods of the CoreLib it names`` (which : AuditedCoreLib) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        let summarised =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    match Assumption.summarises corelib handle with
                    | Some assumption -> yield assumption, method
                    | None -> ()
            ]

        // `JitOptimises`, `MembersAccessible`, `FieldsWrittenVisibly`, `MetadataWellFormed` and
        // `NoThreadAborts` stand in for no method.
        summarised
        |> List.map fst
        |> Set.ofList
        |> shouldEqual (
            Assumption.all
            - set
                [
                    Assumption.JitOptimises
                    Assumption.MembersAccessible
                    Assumption.FieldsWrittenVisibly
                    Assumption.MetadataWellFormed
                    Assumption.NoThreadAborts
                ]
        )

        // Each stands in for one method, but the one about CoreLib's type initializers, which
        // stands in for exactly those.
        let initializers =
            corelib.Methods.Values
            |> Seq.filter (fun method -> method.IsStatic && method.Name = ".cctor")
            |> Seq.length

        for assumption in Assumption.all do
            let count = summarised |> List.filter (fun (a, _) -> a = assumption) |> List.length

            match assumption with
            | Assumption.CoreLibTypeInitializers -> count |> shouldEqual initializers
            | Assumption.NamedTypesLoad
            | Assumption.StackTracePreserved
            | Assumption.DynamicCastsAsDocumented -> count |> shouldEqual 1
            | Assumption.JitOptimises
            | Assumption.MembersAccessible
            | Assumption.FieldsWrittenVisibly
            | Assumption.MetadataWellFormed
            | Assumption.NoThreadAborts -> count |> shouldEqual 0

        for assumption, method in summarised do
            let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]
            let name = $"%s{declaringType.Namespace}.%s{declaringType.Name}::%s{method.Name}"

            match assumption with
            | Assumption.NamedTypesLoad ->
                name |> shouldEqual "System.RuntimeTypeHandle::GetConstraints"
                method.IsStatic |> shouldEqual true

                match method.Body, method.TryNativeImport with
                | MethodBody.PInvoke, Some import ->
                    import.ModuleName |> shouldEqual "QCall"
                    import.EntryPointName |> shouldEqual "RuntimeTypeHandle_GetConstraints"
                | other -> failwith $"%A{assumption} summarises a method that is not a QCall: %A{other}"
            | Assumption.StackTracePreserved ->
                // What CoreCLR's `ExceptionPreserveStackTrace` calls on an exception it throws again.
                name |> shouldEqual "System.Exception::InternalPreserveStackTrace"
                method.IsStatic |> shouldEqual false
                method.IsVirtual |> shouldEqual false
                method.Signature.ParameterTypes |> shouldEqual []
                method.Signature.ReturnType |> shouldEqual MethodReturnType.Void

                match method.Body with
                | MethodBody.Il _ -> ()
                | other -> failwith $"%A{assumption} summarises a method with no IL: %A{other}"
            | Assumption.CoreLibTypeInitializers ->
                method.Name |> shouldEqual ".cctor"
                method.IsStatic |> shouldEqual true
            | Assumption.DynamicCastsAsDocumented ->
                // What CoreCLR's `DynamicInterfaceCastable::IsInstanceOf` calls, as `corelib.h`
                // binds `METHOD__DYNAMICINTERFACECASTABLEHELPERS__IS_INTERFACE_IMPLEMENTED`.
                name
                |> shouldEqual "System.Runtime.InteropServices.DynamicInterfaceCastableHelpers::IsInterfaceImplemented"

                method.IsStatic |> shouldEqual true
                method.Signature.ParameterTypes.Length |> shouldEqual 3

                method.Signature.ReturnType
                |> shouldEqual (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Boolean))

                match method.Body with
                | MethodBody.Il _ -> ()
                | other -> failwith $"%A{assumption} summarises a method with no IL: %A{other}"
            | Assumption.JitOptimises
            | Assumption.MembersAccessible
            | Assumption.FieldsWrittenVisibly
            | Assumption.MetadataWellFormed
            | Assumption.NoThreadAborts -> failwith $"%A{assumption} summarises %s{name}"

    [<TestCaseSource(nameof coreLibs)>]
    let ``the resource lookup's contract stands in for one method of the CoreLib, the one it names``
        (which : AuditedCoreLib)
        : unit
        =
        let corelib = TestIntrinsicBody.coreLib which

        let recognised =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    if ResourceLookup.isLookup corelib handle then
                        yield method
            ]

        let method = recognised |> List.exactlyOne
        let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

        $"%s{declaringType.Namespace}.%s{declaringType.Name}::%s{method.Name}"
        |> shouldEqual "System.SR::InternalGetResourceString"

        method.IsStatic |> shouldEqual true

        method.Signature.ParameterTypes
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.String ]

        method.Signature.ReturnType
        |> shouldEqual (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String))

        match method.Body with
        | MethodBody.Il _ -> ()
        | other -> failwith $"The resource lookup's contract stands in for a method whose body is %A{other}, not IL"

    [<TestCaseSource(nameof coreLibs)>]
    let ``every exception a contract raises is a CoreLib exception type`` (which : AuditedCoreLib) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        let contracts =
            [
                for assumption in Assumption.all do
                    yield $"%A{assumption}", Assumption.raises assumption
                yield "The resource lookup", ResourceLookup.raises
                for runtime in EmulatedRuntime.supported do
                    yield $"Object.GetType's contract on %O{runtime}", ObjectGetType.raises runtime
            ]

        for contract, raises in contracts do
            for name in raises do
                match corelib.TryGetTopLevelTypeDef name.Namespace name.Name with
                | Some _ -> ()
                | None -> failwith $"%s{contract} raises %O{name}, which this CoreLib does not declare"

                // The host's CoreLib says what it derives from.
                let hostType = typeof<obj>.Assembly.GetType name.FullName

                if isNull hostType || not (typeof<Exception>.IsAssignableFrom hostType) then
                    failwith $"%s{contract} raises %O{name}, which is not an exception type of the host's CoreLib"

    /// What CoreLib's own caller of the QCall `RuntimeTypeHandle_GetConstraints` raises for the type
    /// `handle` names, on the real runtime: the full name of the exception's type, if it raises one.
    let private constraintsRaise (handle : RuntimeTypeHandle) : string option =
        let getConstraints =
            typeof<RuntimeTypeHandle>
                .GetMethod ("GetConstraints", BindingFlags.Instance ||| BindingFlags.NonPublic, Type.EmptyTypes)

        // Unwrapped, since wrapping makes a `TargetInvocationException`, whose own constructor
        // looks up its message.
        try
            getConstraints.Invoke (box handle, BindingFlags.DoNotWrapExceptions, null, [||], null)
            |> ignore

            None
        with e ->
            Some (e.GetType().FullName)

    [<Test>]
    let ``what listing constraints raises on the real runtime is in the contract that assumes named types load``
        ()
        : unit
        =
        let contract =
            Assumption.raises Assumption.NamedTypesLoad
            |> List.map (fun name -> name.FullName)
            |> Set.ofList

        // A type that is not a generic parameter.
        let raisedHere = constraintsRaise typeof<int>.TypeHandle
        raisedHere |> shouldEqual (Some "System.ArgumentException")
        contract.Contains raisedHere.Value |> shouldEqual true

        // A generic parameter whose constraints are all in CoreLib.
        let parameter = typedefof<Comparer<int>>.GetGenericArguments().[0]
        constraintsRaise parameter.TypeHandle |> shouldEqual None

    /// An object that declines every interface it is asked about, as its documentation allows.
    type private Declining () =
        interface Runtime.InteropServices.IDynamicInterfaceCastable with
            member _.IsInterfaceImplemented (_ : RuntimeTypeHandle, _ : bool) : bool = false

            member _.GetInterfaceImplementation (_ : RuntimeTypeHandle) : RuntimeTypeHandle = RuntimeTypeHandle ()

    /// What CoreLib's `DynamicInterfaceCastableHelpers.IsInterfaceImplemented`, which CoreCLR calls
    /// to ask an `IDynamicInterfaceCastable`, raises on the real runtime asking `castable` about
    /// `IDisposable`: the full name of the exception's type, if it raises one.
    let private dynamicCastRaises (castable : obj) (throwIfNotImplemented : bool) : string option =
        let helper =
            typeof<obj>.Assembly
                .GetType("System.Runtime.InteropServices.DynamicInterfaceCastableHelpers", true)
                .GetMethod ("IsInterfaceImplemented", BindingFlags.Static ||| BindingFlags.NonPublic)

        // Unwrapped, since wrapping makes a `TargetInvocationException`, whose own constructor
        // looks up its message.
        try
            helper.Invoke (
                (null : obj),
                BindingFlags.DoNotWrapExceptions,
                null,
                [| castable ; box typeof<IDisposable> ; box throwIfNotImplemented |],
                null
            )
            |> ignore<obj>

            None
        with e ->
            Some (e.GetType().FullName)

    [<Test>]
    let ``what asking an IDynamicInterfaceCastable raises on the real runtime is in the contract that assumes it answers as documented``
        ()
        : unit
        =
        let contract =
            Assumption.raises Assumption.DynamicCastsAsDocumented
            |> List.map (fun name -> name.FullName)
            |> Set.ofList

        // Declining where asked to throw: CoreLib throws in the object's place.
        let raised = dynamicCastRaises (Declining ()) true
        raised |> shouldEqual (Some "System.InvalidCastException")
        contract.Contains raised.Value |> shouldEqual true

        dynamicCastRaises (Declining ()) false |> shouldEqual None

        // CoreCLR passes it only an object, but another caller may pass null.
        let raised = dynamicCastRaises (null : obj) false
        raised |> shouldEqual (Some "System.NullReferenceException")
        contract.Contains raised.Value |> shouldEqual true

    /// The invariant culture, until its name cannot be read: setting the current UI culture reads
    /// it once.
    type private NamelessCulture () =
        inherit CultureInfo ("")
        member val Nameless : bool = false with get, set

        override this.Name : string =
            if this.Nameless then
                raise (TimeZoneNotFoundException "a culture's name")
            else
                base.Name

    [<Test>]
    let ``listing a type's constraints on the real runtime can raise what the resource lookup raises`` () : unit =
        // The runtime makes the ArgumentException it raises with its parameterless constructor,
        // which looks up its message under the current UI culture. So the contract alone does not
        // say what escapes; following that constructor (`Assumption.constructs`) does. (A culture
        // like this one is outside what the lookup's own contract answers for.)
        let previous = CultureInfo.CurrentUICulture

        let raised =
            try
                let culture = new NamelessCulture ()
                CultureInfo.CurrentUICulture <- culture
                culture.Nameless <- true

                try
                    constraintsRaise typeof<int>.TypeHandle
                finally
                    culture.Nameless <- false
            finally
                CultureInfo.CurrentUICulture <- previous

        raised |> shouldEqual (Some "System.TimeZoneNotFoundException")

        Assumption.constructs Assumption.NamedTypesLoad
        |> List.map (fun name -> name.FullName)
        |> shouldEqual [ "System.ArgumentException" ]
