namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `LayoutFacts.ofValueType` checked against the host's CoreCLR, over generated value types.
///
/// The host exposes three of the facts more or less directly, and the rest only in combination:
/// * `IsBlittable`, for a type that is neither generic nor an enum, and holds no GC pointers (whose
///   cleanup stub would free what a dirty reference slot points at): only the struct stub of a
///   non-blittable type runs a Cleanup pass, and that zeroes the image, so
///   `Marshal.DestroyStructure` over a dirty buffer leaves it dirty exactly when the type is
///   blittable; and `Marshal.SizeOf` refuses only a type that is not blittable.
/// * `ContainsGcPointers`: `GCHandle.Alloc` pins a boxed value exactly when its `MethodTable` does
///   not `ContainsGCPointers` (`Marshal.IsPinnable`).
/// * Everything else, through `RuntimeHelpers.PrepareMethod` on an `[UnmanagedCallersOnly]` method
///   taking the type, with runtime marshalling on and off: CoreCLR raises
///   `InvalidProgramException` exactly when `PInvoke::MarshalingRequired` holds
///   (`COMDelegate::ThrowIfInvalidUnmanagedCallersOnlyUsage`). That verdict combines the facts by
///   the rule `marshalingRequired` mirrors.
///
/// The oracle is the test host's own runtime, in process, over the test host's own CoreLib
/// (`HostImage`): PawPrint reads the image the host runs. Run the test host on another runtime
/// (`DOTNET_ROLL_FORWARD=LatestMajor`, with `DOTNET_ROLL_FORWARD_TO_PRERELEASE=1` for a preview) to
/// check that runtime.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLayoutFacts =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelib : DumpedAssembly =
        Assembly.readFile loggerFactory HostImage.coreLibPath

    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private baseState : Lazy<TypeSystemState> =
        lazy
            (let loaded = LoadedAssemblies.ofAssemblies [ corelib ]

             { TypeSystemState.Empty with
                 _LoadedAssemblies = loaded
                 ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
             })

    /// CoreLib's IEEE 754 decimal floating-point types, which .NET 11 adds. CoreCLR refuses to
    /// marshal them by value, as it does `Int128`.
    let private decimalFloatingPointTypes : Type list =
        [ "Decimal32" ; "Decimal64" ; "Decimal128" ]
        |> List.choose (fun name -> typeof<obj>.Assembly.GetType ("System.Numerics." + name) |> Option.ofObj)

    /// The CoreLib value types generated as fields: every primitive arm of `IsFieldBlittable`, and
    /// the CoreLib types CoreCLR treats specially.
    let private valueTypeLeaves : Type list =
        [
            typeof<int>
            typeof<int64>
            typeof<byte>
            typeof<int16>
            typeof<float32>
            typeof<float>
            typeof<nativeint>
            typeof<bool>
            typeof<char>
            typeof<decimal>
            typeof<DateTime>
            typeof<Int128>
        ]
        @ decimalFloatingPointTypes

    /// `delegate*<void>`, which reflection cannot construct but can read off a field C# declares.
    let private functionPointerType : Lazy<Type> =
        lazy
            (let image =
                Roslyn.compileAssembly
                    "LayoutFactsFunctionPointer"
                    Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                    []
                    [ "public unsafe struct Holder { public delegate*<void> F; }" ]

             (System.Reflection.Assembly.Load image).GetType("Holder").GetField("F").FieldType)

    [<RequireQualifiedAccess>]
    type private Layout =
        | Sequential
        | Auto
        | Explicit

    [<RequireQualifiedAccess>]
    type private CharSetChoice =
        | Ansi
        | Unicode
        | Auto

    [<RequireQualifiedAccess>]
    type private EnumOver =
        | Int32
        | Byte
        | Char
        | Boolean

    [<RequireQualifiedAccess>]
    type private BufferElement =
        | Byte
        | Char
        | Int32

    /// One of the assembly's generic wrappers `Wrap<T> { T X; }`, which differ in layout and charset.
    [<RequireQualifiedAccess>]
    type private Wrapper =
        | SequentialAnsi
        | SequentialUnicode
        | Auto

    [<RequireQualifiedAccess>]
    type private Kind =
        /// A CoreLib type: a primitive, `decimal`, `DateTime`, `Int128`, `string`, or a decimal
        /// floating-point type.
        | Named of Type
        | Enum of EnumOver
        /// `int*`.
        | Pointer
        /// `delegate*<void>`.
        | FunctionPointer
        | Struct of Shape
        | Generic of Wrapper * Kind
        /// What C# emits for a `fixed` buffer: a sequential struct of the buffer's size, holding one
        /// element.
        | FixedBuffer of BufferElement * length : int * CharSetChoice
        /// `[InlineArray(length)]` over one field, of sequential layout as C# emits it. CoreCLR refuses
        /// explicit layout on one, and the host refuses some of auto layout as corrupt metadata.
        | InlineArray of Kind * length : int * CharSetChoice

    and private Field =
        {
            Kind : Kind
            MarshalAs : UnmanagedType option
        }

    and private Shape =
        {
            Layout : Layout
            CharSet : CharSetChoice
            Fields : Field list
        }

    /// `[MarshalAs]` values over every arm of `IsFieldBlittable` (fieldmarshaler.cpp), and some
    /// that no arm accepts.
    let private marshalAsChoices : UnmanagedType list =
        [
            UnmanagedType.I1
            UnmanagedType.U1
            UnmanagedType.I2
            UnmanagedType.U2
            UnmanagedType.I4
            UnmanagedType.U4
            UnmanagedType.I8
            UnmanagedType.U8
            UnmanagedType.R4
            UnmanagedType.R8
            UnmanagedType.SysInt
            UnmanagedType.SysUInt
            UnmanagedType.Bool
            UnmanagedType.Struct
            UnmanagedType.Error
            UnmanagedType.FunctionPtr
            UnmanagedType.LPStr
        ]

    /// For a field of `kind`, the `[MarshalAs]` values `IsFieldBlittable` accepts for its element
    /// type, and one it does not: each arm's own list, which a uniform choice from
    /// `marshalAsChoices` meets too rarely to test.
    let private marshalAsNear (kind : Kind) : UnmanagedType list =
        match kind with
        | Kind.Named t when t = typeof<int> ->
            [ UnmanagedType.I4 ; UnmanagedType.U4 ; UnmanagedType.Error ; UnmanagedType.I8 ]
        | Kind.Enum EnumOver.Int32 -> [ UnmanagedType.I4 ; UnmanagedType.U4 ; UnmanagedType.Error ; UnmanagedType.I2 ]
        | Kind.Named t when t = typeof<int64> -> [ UnmanagedType.I8 ; UnmanagedType.U8 ; UnmanagedType.I4 ]
        | Kind.Named t when t = typeof<byte> -> [ UnmanagedType.I1 ; UnmanagedType.U1 ; UnmanagedType.I2 ]
        | Kind.Enum EnumOver.Byte -> [ UnmanagedType.I1 ; UnmanagedType.U1 ; UnmanagedType.Bool ]
        | Kind.Named t when t = typeof<int16> -> [ UnmanagedType.I2 ; UnmanagedType.U2 ; UnmanagedType.I4 ]
        | Kind.Named t when t = typeof<float32> -> [ UnmanagedType.R4 ; UnmanagedType.R8 ]
        | Kind.Named t when t = typeof<float> -> [ UnmanagedType.R8 ; UnmanagedType.R4 ]
        | Kind.Named t when t = typeof<nativeint> -> [ UnmanagedType.SysInt ; UnmanagedType.SysUInt ; UnmanagedType.I8 ]
        | Kind.Named t when t = typeof<char> -> [ UnmanagedType.I2 ; UnmanagedType.U2 ; UnmanagedType.U1 ]
        | Kind.Enum EnumOver.Char -> [ UnmanagedType.I2 ; UnmanagedType.U2 ; UnmanagedType.I1 ]
        | Kind.Named t when t = typeof<bool> -> [ UnmanagedType.Bool ; UnmanagedType.U1 ; UnmanagedType.I1 ]
        | Kind.Enum EnumOver.Boolean -> [ UnmanagedType.U1 ; UnmanagedType.I1 ]
        | Kind.Pointer -> [ UnmanagedType.SysInt ; UnmanagedType.I8 ]
        | Kind.FunctionPointer -> [ UnmanagedType.FunctionPtr ; UnmanagedType.SysInt ]
        | Kind.Named t when t = typeof<string> -> [ UnmanagedType.LPStr ; UnmanagedType.Struct ]
        | Kind.Named _
        | Kind.Struct _
        | Kind.Generic _
        | Kind.FixedBuffer _
        | Kind.InlineArray _ -> [ UnmanagedType.Struct ; UnmanagedType.I4 ]

    /// Per-case weights, themselves generated, so that runs cover sparse and dense regimes.
    type private Bias =
        {
            MarshalAsWeight : int
            NestingWeight : int
            /// The most fields a generated struct has. With few, a struct's verdicts turn on each one.
            MaxFields : int
        }

    let private genCharSet : Gen<CharSetChoice> =
        Gen.elements [ CharSetChoice.Ansi ; CharSetChoice.Unicode ; CharSetChoice.Auto ]

    let rec private genKind (bias : Bias) (depth : int) (allowPointer : bool) (allowReference : bool) : Gen<Kind> =
        let leaves =
            [
                yield! valueTypeLeaves |> List.map Kind.Named
                yield Kind.Enum EnumOver.Int32
                yield Kind.Enum EnumOver.Byte
                yield Kind.Enum EnumOver.Char
                yield Kind.Enum EnumOver.Boolean
                if allowPointer then
                    yield Kind.Pointer
                    yield Kind.FunctionPointer
                if allowReference then
                    yield Kind.Named typeof<string>
            ]

        let fixedBuffer =
            gen {
                let! element = Gen.elements [ BufferElement.Byte ; BufferElement.Char ; BufferElement.Int32 ]
                let! length = Gen.choose (1, 4)
                let! charSet = genCharSet
                return Kind.FixedBuffer (element, length, charSet)
            }

        if depth <= 0 then
            Gen.frequency [ 6, Gen.elements leaves ; 1, fixedBuffer ]
        else
            Gen.frequency
                [
                    6, Gen.elements leaves
                    1, fixedBuffer
                    bias.NestingWeight, genShape bias (depth - 1) |> Gen.map Kind.Struct
                    bias.NestingWeight,
                    gen {
                        let! wrapper =
                            Gen.elements [ Wrapper.SequentialAnsi ; Wrapper.SequentialUnicode ; Wrapper.Auto ]
                        // A pointer cannot be a generic argument.
                        let! argument = genKind bias (depth - 1) false true
                        return Kind.Generic (wrapper, argument)
                    }
                    bias.NestingWeight,
                    gen {
                        let! element = genKind bias (depth - 1) true true
                        let! length = Gen.choose (1, 3)
                        let! charSet = genCharSet
                        return Kind.InlineArray (element, length, charSet)
                    }
                ]

    and private genShape (bias : Bias) (depth : int) : Gen<Shape> =
        gen {
            let! layout =
                Gen.frequency
                    [
                        3, Gen.constant Layout.Sequential
                        1, Gen.constant Layout.Auto
                        1, Gen.constant Layout.Explicit
                    ]

            let! charSet = genCharSet
            let! count = Gen.choose (0, bias.MaxFields)

            let genField =
                gen {
                    let! kind = genKind bias depth true true

                    let! marshalAs =
                        Gen.frequency
                            [
                                4, Gen.constant None
                                bias.MarshalAsWeight, Gen.elements marshalAsChoices |> Gen.map Some
                                2 * bias.MarshalAsWeight, Gen.elements (marshalAsNear kind) |> Gen.map Some
                            ]

                    return
                        {
                            Kind = kind
                            MarshalAs = marshalAs
                        }
                }

            let! fields = Gen.listOfLength count genField

            return
                {
                    Layout = layout
                    CharSet = charSet
                    Fields = fields
                }
        }

    /// The type an `[UnmanagedCallersOnly]` method takes: a value type, mostly a generated struct.
    let private genTop : Gen<Kind> =
        gen {
            let! marshalAsWeight = Gen.choose (0, 4)
            let! nestingWeight = Gen.choose (1, 4)
            let! maxFields = Gen.choose (1, 4)

            let bias =
                {
                    MarshalAsWeight = marshalAsWeight
                    NestingWeight = nestingWeight
                    MaxFields = maxFields
                }

            return!
                Gen.frequency
                    [
                        4, genShape bias 2 |> Gen.map Kind.Struct
                        1, genKind bias 2 false false
                        // An enum parameter, which `IsEnum` exempts; a field of enum type is judged as its primitive.
                        1,
                        Gen.elements [ EnumOver.Int32 ; EnumOver.Byte ; EnumOver.Char ; EnumOver.Boolean ]
                        |> Gen.map Kind.Enum
                    ]
        }

    /// An upper bound on the size of a value of `kind`, rounded up to 16, so that consecutive
    /// explicit offsets that far apart never overlap and keep every reference aligned.
    let rec private sizeBound (kind : Kind) : int =
        let round16 (n : int) = max 16 ((n + 15) / 16 * 16)

        match kind with
        | Kind.Named _
        | Kind.Enum _
        | Kind.Pointer
        | Kind.FunctionPointer -> 16
        | Kind.Struct shape -> shape.Fields |> List.sumBy (fun field -> sizeBound field.Kind) |> round16
        | Kind.Generic (_, argument) -> sizeBound argument
        | Kind.FixedBuffer (_, length, _) -> round16 (4 * length)
        | Kind.InlineArray (element, length, _) -> length * sizeBound element

    let private charSetAttributes (charSet : CharSetChoice) : TypeAttributes =
        match charSet with
        | CharSetChoice.Ansi -> TypeAttributes.AnsiClass
        | CharSetChoice.Unicode -> TypeAttributes.UnicodeClass
        | CharSetChoice.Auto -> TypeAttributes.AutoClass

    let private structAttributes (layout : Layout) (charSet : CharSetChoice) : TypeAttributes =
        let layout =
            match layout with
            | Layout.Sequential -> TypeAttributes.SequentialLayout
            | Layout.Auto -> TypeAttributes.AutoLayout
            | Layout.Explicit -> TypeAttributes.ExplicitLayout

        TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| layout
        ||| charSetAttributes charSet

    let private attribute (ty : Type) (argumentTypes : Type[]) (arguments : obj[]) : CustomAttributeBuilder =
        match ty.GetConstructor argumentTypes with
        | null -> failwith $"%s{ty.FullName} has no constructor over %A{argumentTypes}"
        | ctor -> CustomAttributeBuilder (ctor, arguments)

    /// An assembly holding the types `top` names, and `Ucos`, whose `[UnmanagedCallersOnly]`
    /// methods `Direct` and `Wrapped` take `top` and `Wrap<top>` (the sequential, ANSI wrapper).
    let private emit (name : string) (disableRuntimeMarshalling : bool) (top : Kind) : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        if disableRuntimeMarshalling then
            builder.SetCustomAttribute (attribute typeof<DisableRuntimeMarshallingAttribute> [||] [||])

        let md = builder.DefineDynamicModule name
        let mutable counter = 0

        let fresh (prefix : string) : string =
            counter <- counter + 1
            $"%s{prefix}%d{counter}"

        let enums = Dictionary<EnumOver, Type> ()

        let enumType (over : EnumOver) : Type =
            match enums.TryGetValue over with
            | true, t -> t
            | false, _ ->
                let underlying =
                    match over with
                    | EnumOver.Int32 -> typeof<int>
                    | EnumOver.Byte -> typeof<byte>
                    | EnumOver.Char -> typeof<char>
                    | EnumOver.Boolean -> typeof<bool>

                let e = md.DefineEnum (fresh "E", TypeAttributes.Public, underlying)
                e.DefineLiteral ("One", Convert.ChangeType (1, underlying)) |> ignore
                let t = e.CreateType ()
                enums.[over] <- t
                t

        let wrappers = Dictionary<Wrapper, Type> ()

        let wrapperType (wrapper : Wrapper) : Type =
            match wrappers.TryGetValue wrapper with
            | true, t -> t
            | false, _ ->
                let layout, charSet =
                    match wrapper with
                    | Wrapper.SequentialAnsi -> Layout.Sequential, CharSetChoice.Ansi
                    | Wrapper.SequentialUnicode -> Layout.Sequential, CharSetChoice.Unicode
                    | Wrapper.Auto -> Layout.Auto, CharSetChoice.Ansi

                let tb =
                    md.DefineType (fresh "Wrap", structAttributes layout charSet, typeof<ValueType>)

                let parameters = tb.DefineGenericParameters [| "T" |]
                tb.DefineField ("X", parameters.[0], FieldAttributes.Public) |> ignore
                let t = tb.CreateType ()
                wrappers.[wrapper] <- t
                t

        let rec typeOf (kind : Kind) : Type =
            match kind with
            | Kind.Named t -> t
            | Kind.Enum over -> enumType over
            | Kind.Pointer -> typeof<int>.MakePointerType ()
            | Kind.FunctionPointer -> functionPointerType.Force ()
            | Kind.Struct shape ->
                let fieldTypes = shape.Fields |> List.map (fun field -> typeOf field.Kind)

                let tb =
                    md.DefineType (fresh "S", structAttributes shape.Layout shape.CharSet, typeof<ValueType>)

                let mutable offset = 0

                List.zip shape.Fields fieldTypes
                |> List.iteri (fun i (field, fieldType) ->
                    let fb = tb.DefineField ($"F%d{i}", fieldType, FieldAttributes.Public)

                    match field.MarshalAs with
                    | None -> ()
                    | Some u ->
                        fb.SetCustomAttribute (
                            attribute typeof<MarshalAsAttribute> [| typeof<UnmanagedType> |] [| box u |]
                        )

                    if shape.Layout = Layout.Explicit then
                        fb.SetOffset offset
                        offset <- offset + sizeBound field.Kind
                )

                tb.CreateType ()
            | Kind.Generic (wrapper, argument) -> (wrapperType wrapper).MakeGenericType (typeOf argument)
            | Kind.FixedBuffer (element, length, charSet) ->
                let elementType, elementSize =
                    match element with
                    | BufferElement.Byte -> typeof<byte>, 1
                    | BufferElement.Char -> typeof<char>, 2
                    | BufferElement.Int32 -> typeof<int>, 4

                let tb =
                    md.DefineType (
                        fresh "Buffer",
                        structAttributes Layout.Sequential charSet,
                        typeof<ValueType>,
                        PackingSize.Unspecified,
                        elementSize * length
                    )

                tb.DefineField ("FixedElementField", elementType, FieldAttributes.Public)
                |> ignore

                tb.CreateType ()
            | Kind.InlineArray (element, length, charSet) ->
                let elementType = typeOf element

                let tb =
                    md.DefineType (fresh "Inline", structAttributes Layout.Sequential charSet, typeof<ValueType>)

                tb.SetCustomAttribute (attribute typeof<InlineArrayAttribute> [| typeof<int> |] [| box length |])
                tb.DefineField ("Element", elementType, FieldAttributes.Public) |> ignore
                tb.CreateType ()

        let topType = typeOf top
        let wrappedType = (wrapperType Wrapper.SequentialAnsi).MakeGenericType topType

        let ucos =
            md.DefineType ("Ucos", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        for methodName, parameter in [ "Direct", topType ; "Wrapped", wrappedType ] do
            let mb =
                ucos.DefineMethod (
                    methodName,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [| parameter |]
                )

            mb.SetCustomAttribute (attribute typeof<UnmanagedCallersOnlyAttribute> [||] [||])
            mb.GetILGenerator().Emit OpCodes.Ret

        ucos.CreateType () |> ignore

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// What the host's CoreCLR says about one generated case.
    type private HostAnswer =
        {
            /// Whether `PrepareMethod` refuses `Direct`, then `Wrapped`, with runtime marshalling on.
            RequiredOn : bool * bool
            /// The same, with runtime marshalling off.
            RequiredOff : bool * bool
            /// Whether `top` is blittable, where the host can say: not for a generic type, an enum, or a
            /// type holding GC pointers, whose cleanup stub would free what a dirty buffer points at.
            Blittable : bool option
            /// Whether a boxed `top` can be pinned.
            Pinnable : bool
        }

    /// Whether compiling `method` raises `InvalidProgramException`. Any other failure fails the test:
    /// it means the generated assembly is not one CoreCLR loads.
    let private refused (method : System.Reflection.MethodInfo) : bool =
        try
            RuntimeHelpers.PrepareMethod method.MethodHandle
            false
        with :? InvalidProgramException ->
            true

    let private hostBlittable (pinnable : bool) (t : Type) : bool option =
        if t.IsGenericType || t.IsEnum || not pinnable then
            None
        else

        match
            (try
                Some (Marshal.SizeOf t)
             with :? ArgumentException ->
                 None)
        with
        | None -> Some false
        | Some size ->
            let dirty = 0xABuy
            let buffer = Marshal.AllocHGlobal size

            try
                for i in 0 .. size - 1 do
                    Marshal.WriteByte (buffer, i, dirty)

                // A blittable type has no struct stub, so this does nothing to its image; a
                // non-blittable type's stub may raise instead of cleaning, which says the same.
                try
                    Marshal.DestroyStructure (buffer, t)
                    Some ([ 0 .. size - 1 ] |> List.forall (fun i -> Marshal.ReadByte (buffer, i) = dirty))
                with _ ->
                    Some false
            finally
                Marshal.FreeHGlobal buffer

    let private hostPinnable (t : Type) : bool =
        let boxed = RuntimeHelpers.GetUninitializedObject t

        try
            let handle = GCHandle.Alloc (boxed, GCHandleType.Pinned)
            handle.Free ()
            true
        with :? ArgumentException ->
            false

    let private hostAnswer (on : byte[]) (off : byte[]) : HostAnswer =
        let context =
            Runtime.Loader.AssemblyLoadContext ("LayoutFacts", isCollectible = true)

        try
            let ucosOf (image : byte[]) : Type =
                (context.LoadFromStream (new MemoryStream (image))).GetType "Ucos"
                |> Option.ofObj
                |> Option.defaultWith (fun () -> failwith "the generated assembly has no Ucos")

            let verdicts (ucos : Type) : bool * bool =
                refused (ucos.GetMethod "Direct"), refused (ucos.GetMethod "Wrapped")

            let ucosOn = ucosOf on
            let ucosOff = ucosOf off
            let top = (ucosOn.GetMethod "Direct").GetParameters().[0].ParameterType
            let pinnable = hostPinnable top

            {
                RequiredOn = verdicts ucosOn
                RequiredOff = verdicts ucosOff
                Blittable = hostBlittable pinnable top
                Pinnable = pinnable
            }
        finally
            context.Unload ()

    /// PawPrint's facts for the types `Direct` and `Wrapped` take, in the assembly `image`.
    let private pawPrintFacts (name : string) (image : byte[]) : ValueTypeLayoutFacts * ValueTypeLayoutFacts =
        let dumped =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let state = baseState.Force().WithLoadedAssembly dumped

        let parameterOf (methodName : string) : TypeDefn =
            dumped.Methods.Values
            |> Seq.filter (fun m -> m.Name = methodName)
            |> Seq.exactlyOne
            |> fun m -> List.exactlyOne m.Signature.ParameterTypes

        let factsOf (state : TypeSystemState) (methodName : string) : TypeSystemState * ValueTypeLayoutFacts =
            let state, handle =
                TypeSystemState.concretizeType
                    loggerFactory
                    HostImage.runtimeDirs
                    bct
                    state
                    dumped.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (parameterOf methodName)
                |> BindingResult.bound

            let state, facts =
                LayoutFacts.ofValueType loggerFactory HostImage.runtimeDirs bct state handle
                |> BindingResult.bound

            match facts with
            | Some facts -> state, facts
            | None -> failwith $"LayoutFacts says the parameter of %s{methodName} is not a value type"

        let state, direct = factsOf state "Direct"
        let _, wrapped = factsOf state "Wrapped"
        direct, wrapped

    /// `PInvoke::MarshalingRequired` (dllimport.cpp:3322 on .NET 10, 3450 on .NET 11) for one
    /// parameter of value type, from the facts it reads.
    ///
    /// `IsValidForGenericMarshalling` (mlinfo.cpp:582) also refuses an instantiation of `Nullable`,
    /// `Span`, `ReadOnlySpan` or a vector type; no generated case is one, so `instantiation` says
    /// only whether the type is generic. On .NET 10 CoreCLR does not read the decimal
    /// floating-point fact, which is false there because CoreLib has no such type.
    let private marshalingRequired (runtimeMarshalling : bool) (instantiation : bool) (facts : ValueTypeLayoutFacts) =
        let validForGenericMarshalling =
            not instantiation || not runtimeMarshalling || facts.IsBlittable

        not validForGenericMarshalling
        || facts.IsInt128OrHasInt128Fields
        || facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields
        || (if runtimeMarshalling then
                not facts.IsBlittable && not facts.IsEnum
            else
                not facts.IsEnum
                && (facts.ContainsGcPointers || facts.IsAutoLayoutOrHasAutoLayoutField))

    /// Emit `top` into assemblies named after `name`, and fail unless the host's answers are those
    /// PawPrint's facts predict; return the host's answers and the facts of `top`.
    let private check (name : string) (top : Kind) : HostAnswer * ValueTypeLayoutFacts =
        let on = emit (name + "On") false top
        let off = emit (name + "Off") true top
        let host = hostAnswer on off
        let direct, wrapped = pawPrintFacts (name + "On") on
        let directOff, wrappedOff = pawPrintFacts (name + "Off") off

        let topIsGeneric =
            match top with
            | Kind.Generic _ -> true
            | _ -> false

        let expected =
            {
                RequiredOn = marshalingRequired true topIsGeneric direct, marshalingRequired true true wrapped
                RequiredOff = marshalingRequired false topIsGeneric directOff, marshalingRequired false true wrappedOff
                Blittable = host.Blittable |> Option.map (fun _ -> direct.IsBlittable)
                Pinnable = not direct.ContainsGcPointers
            }

        if (direct, wrapped) <> (directOff, wrappedOff) then
            failwith
                $"the facts depend on DisableRuntimeMarshalling: %A{(direct, wrapped)} against %A{(directOff, wrappedOff)}"

        if host <> expected then
            failwith
                $"%A{top}\n  host:     %A{host}\n  PawPrint: %A{expected}\n  facts of Direct: %A{direct}\n  facts of Wrapped: %A{wrapped}"

        host, direct

    [<Test>]
    let ``Layout facts agree with the host's CoreCLR over generated value types`` () : unit =
        let mutable cases = 0
        let observed = Dictionary<string, int> ()

        let count (key : string) : unit =
            observed.[key] <- observed.GetValueOrDefault key + 1

        let property (top : Kind) : unit =
            cases <- cases + 1
            let host, direct = check $"LayoutFacts%d{cases}" top

            count $"requiredOn=%b{fst host.RequiredOn}"
            count $"requiredOff=%b{fst host.RequiredOff}"
            count $"wrappedOn=%b{snd host.RequiredOn}"
            count $"blittable=%A{host.Blittable}"
            count $"pinnable=%b{host.Pinnable}"
            count $"hasLayout=%b{direct.HasLayout}"
            count $"autoLayoutOrHasAutoLayoutField=%b{direct.IsAutoLayoutOrHasAutoLayoutField}"
            count $"int128=%b{direct.IsInt128OrHasInt128Fields}"
            count $"decimalFloatingPoint=%b{direct.IsDecimalFloatingPointOrHasDecimalFloatingPointFields}"
            count $"enum=%b{direct.IsEnum}"

            if fst host.RequiredOn <> fst host.RequiredOff then
                count "onDiffersFromOff"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll (Arb.fromGen genTop) property)

        let seen (key : string) : int = observed.GetValueOrDefault key

        // Each answer the host gives, and each fact, was exercised both ways. The rarest, a case
        // holding `Int128` and one holding GC pointers, are each about 5% of cases on either runtime, so
        // fewer than 5 of 1000 is a Poisson tail below 1e-14.
        for key in
            [
                "requiredOn=true"
                "requiredOn=false"
                "requiredOff=true"
                "requiredOff=false"
                "wrappedOn=true"
                "wrappedOn=false"
                "blittable=Some true"
                "blittable=Some false"
                "pinnable=true"
                "pinnable=false"
                "hasLayout=true"
                "hasLayout=false"
                "autoLayoutOrHasAutoLayoutField=true"
                "autoLayoutOrHasAutoLayoutField=false"
                "int128=true"
                "int128=false"
                "enum=true"
                "enum=false"
                "onDiffersFromOff"
            ] do
            if seen key < 5 then
                failwith $"only %d{seen key} of %d{cases} cases had %s{key}: %A{observed}"

        // The decimal floating-point fact is exercised exactly where the runtime has the types.
        let decimalFloatingPoint = seen "decimalFloatingPoint=true"

        if decimalFloatingPointTypes.IsEmpty then
            decimalFloatingPoint |> shouldEqual 0
        elif decimalFloatingPoint < 5 then
            failwith $"only %d{decimalFloatingPoint} cases held decimal floating point: %A{observed}"

    /// Every leaf field kind under every `[MarshalAs]` value (and none), in a sequential struct of
    /// each charset that holds that one field: each arm of `IsFieldBlittable` and each native type it
    /// accepts or refuses, which the generated cases meet too rarely to settle on every run.
    [<Test>]
    let ``Layout facts agree with the host's CoreCLR on every one-field struct`` () : unit =
        let leaves =
            [
                yield! valueTypeLeaves |> List.map Kind.Named
                yield Kind.Named typeof<string>
                yield Kind.Enum EnumOver.Int32
                yield Kind.Enum EnumOver.Byte
                yield Kind.Enum EnumOver.Char
                yield Kind.Enum EnumOver.Boolean
                yield Kind.Pointer
                yield Kind.FunctionPointer
            ]

        let answers =
            [
                for kind in leaves do
                    for marshalAs in None :: List.map Some marshalAsChoices do
                        for charSet in [ CharSetChoice.Ansi ; CharSetChoice.Unicode ; CharSetChoice.Auto ] do
                            yield
                                Kind.Struct
                                    {
                                        Layout = Layout.Sequential
                                        CharSet = charSet
                                        Fields =
                                            [
                                                {
                                                    Kind = kind
                                                    MarshalAs = marshalAs
                                                }
                                            ]
                                    }
            ]
            |> List.mapi (fun i top -> check $"LayoutFactsSweep%d{i}" top)

        // Both answers came up, so the sweep separated blittable fields from the rest.
        let blittable =
            answers |> List.countBy (fun (host, _) -> host.Blittable) |> Map.ofList

        blittable.GetValueOrDefault (Some true) |> shouldBeGreaterThan 0
        blittable.GetValueOrDefault (Some false) |> shouldBeGreaterThan 0

    /// Enums whose one instance field is not of a primitive type, and a struct holding each:
    /// `SelfEnum`'s field is of `SelfEnum` itself, `CycleA`'s and `CycleB`'s are of each other,
    /// `EnumOfEnum`'s is of an ordinary enum, and `StructEnum`'s is of a struct.
    let private oddEnumCorpus : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "OddEnums", typeof<obj>.Assembly)

        let md = builder.DefineDynamicModule "OddEnums"

        let ordinary =
            (md.DefineEnum ("Ordinary", TypeAttributes.Public, typeof<int>)).CreateType ()

        let plain =
            md.DefineType ("Plain", structAttributes Layout.Sequential CharSetChoice.Ansi, typeof<ValueType>)

        plain.DefineField ("X", typeof<int>, FieldAttributes.Public) |> ignore
        let plain = plain.CreateType ()

        let enums =
            [ "SelfEnum" ; "CycleA" ; "CycleB" ; "EnumOfEnum" ; "StructEnum" ]
            |> List.map (fun name ->
                name, md.DefineType (name, TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<Enum>)
            )
            |> Map.ofList

        for name, underlying in
            [
                "SelfEnum", (enums.["SelfEnum"] :> Type)
                "CycleA", (enums.["CycleB"] :> Type)
                "CycleB", (enums.["CycleA"] :> Type)
                "EnumOfEnum", ordinary
                "StructEnum", plain
            ] do
            enums.[name]
                .DefineField (
                    "value__",
                    underlying,
                    FieldAttributes.Public
                    ||| FieldAttributes.SpecialName
                    ||| FieldAttributes.RTSpecialName
                )
            |> ignore

        for KeyValue (name, tb) in enums do
            tb.CreateType () |> ignore

            let holder =
                md.DefineType ("Holds" + name, structAttributes Layout.Sequential CharSetChoice.Ansi, typeof<ValueType>)

            holder.DefineField ("E", tb, FieldAttributes.Public) |> ignore
            holder.CreateType () |> ignore

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// CoreCLR strips an enum-typed field of an enum down to that enum's underlying type
    /// (`#FieldDescTypeMorph`, methodtablebuilder.cpp), then refuses an enum whose field is still
    /// not of a primitive type. Where the host loads a type the facts must say what it says, and
    /// where it refuses one they must be a failure, not a walk without end.
    [<Test>]
    let ``An enum's field is judged through enums to a primitive, and refused otherwise, as the host does`` () : unit =
        let context = Runtime.Loader.AssemblyLoadContext ("OddEnums", isCollectible = true)

        try
            let hostAssembly = context.LoadFromStream (new MemoryStream (oddEnumCorpus))

            let dumped =
                Assembly.read loggerFactory (Some "OddEnums.dll") (new MemoryStream (oddEnumCorpus))

            let mutable state = baseState.Force().WithLoadedAssembly dumped

            let outcomes =
                [ "SelfEnum" ; "CycleA" ; "CycleB" ; "EnumOfEnum" ; "StructEnum" ]
                |> List.collect (fun name -> [ name ; "Holds" + name ])
                |> List.map (fun name ->
                    // The host may refuse the type as soon as it is named.
                    let hostType =
                        try
                            let hostType = hostAssembly.GetType name
                            RuntimeHelpers.GetUninitializedObject hostType |> ignore
                            Some hostType
                        with :? TypeLoadException ->
                            None

                    let hostLoads = hostType.IsSome

                    let typeInfo =
                        dumped.TypeDefs.Values |> Seq.filter (fun t -> t.Name = name) |> Seq.exactlyOne

                    let next, handle =
                        TypeSystemState.concretizeType
                            loggerFactory
                            HostImage.runtimeDirs
                            bct
                            state
                            dumped.DefinitionFullName
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            (TypeDefn.FromDefinition (
                                typeInfo.Identity,
                                System.Reflection.Metadata.SignatureTypeKind.ValueType
                            ))
                        |> BindingResult.bound

                    let next, facts =
                        LayoutFacts.ofValueType loggerFactory HostImage.runtimeDirs bct next handle

                    state <- next

                    match hostType, facts with
                    | Some hostType, Ok (Some facts) ->
                        if hostType.IsEnum then
                            facts |> shouldEqual ValueTypeLayoutFacts.Enum
                        else
                            Some facts.IsBlittable |> shouldEqual (hostBlittable true hostType)
                    | None, Error (TypeBindingFailure.Unmodelled _) -> ()
                    | _, other -> failwith $"%s{name}: the host loads it %b{hostLoads}, but the facts are %A{other}"

                    name, hostLoads
                )

            // The host draws the line where the comparison above needs it drawn.
            outcomes
            |> shouldEqual
                [
                    "SelfEnum", false
                    "HoldsSelfEnum", false
                    "CycleA", false
                    "HoldsCycleA", false
                    "CycleB", false
                    "HoldsCycleB", false
                    "EnumOfEnum", true
                    "HoldsEnumOfEnum", true
                    "StructEnum", false
                    "HoldsStructEnum", false
                ]
        finally
            context.Unload ()

    /// Byref-like structs, which hold a byref (or a `TypedReference`, or a `Span<T>`) and nothing
    /// that is an object reference, each with `[UnmanagedCallersOnly]` methods `Direct<name>` taking
    /// it. C# refuses to declare such a method, so they are emitted.
    let private byrefLikeCorpus (name : string) (disableRuntimeMarshalling : bool) : byte[] * string list =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        if disableRuntimeMarshalling then
            builder.SetCustomAttribute (attribute typeof<DisableRuntimeMarshallingAttribute> [||] [||])

        let md = builder.DefineDynamicModule name

        let refStruct (typeName : string) (fields : (string * Type) list) : Type =
            let tb =
                md.DefineType (typeName, structAttributes Layout.Sequential CharSetChoice.Ansi, typeof<ValueType>)

            tb.SetCustomAttribute (attribute typeof<IsByRefLikeAttribute> [||] [||])

            for fieldName, fieldType in fields do
                tb.DefineField (fieldName, fieldType, FieldAttributes.Public) |> ignore

            tb.CreateType ()

        // F# cannot name a byref-like type in `typeof`.
        let coreLibType (name : string) : Type =
            typeof<obj>.Assembly.GetType name
            |> Option.ofObj
            |> Option.defaultWith (fun () -> failwith $"CoreLib has no %s{name}")

        let refInt = refStruct "RefInt" [ "X", typeof<int>.MakeByRefType () ]

        let structs =
            [
                refInt
                refStruct "RefIntAndInt" [ "X", typeof<int>.MakeByRefType () ; "Y", typeof<int> ]
                refStruct "RefString" [ "X", typeof<string>.MakeByRefType () ]
                refStruct "HoldsRefInt" [ "R", refInt ]
                refStruct "HoldsTypedReference" [ "T", coreLibType "System.TypedReference" ]
                refStruct "HoldsSpan" [ "S", (coreLibType "System.Span`1").MakeGenericType typeof<int> ]
            ]

        let ucos =
            md.DefineType ("Ucos", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        for s in structs do
            let mb =
                ucos.DefineMethod (
                    "Direct" + s.Name,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [| s |]
                )

            mb.SetCustomAttribute (attribute typeof<UnmanagedCallersOnlyAttribute> [||] [||])
            mb.GetILGenerator().Emit OpCodes.Ret

        ucos.CreateType () |> ignore

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray (), structs |> List.map _.Name

    /// CoreCLR's `ContainsGCPointers` counts the GC series of a type's instances, and a byref field is
    /// in none (a byref-like value is never on the heap), so the facts do not count one; while
    /// `RuntimeHelpers.IsReferenceOrContainsReferences`, which `GcReferences` answers, does.
    [<Test>]
    let ``A byref field holds a reference that is not a GC pointer`` () : unit =
        let corpus =
            [
                false, byrefLikeCorpus "ByrefLikeOn" false
                true, byrefLikeCorpus "ByrefLikeOff" true
            ]

        let context = Runtime.Loader.AssemblyLoadContext ("ByrefLike", isCollectible = true)

        try
            for disabled, (image, names) in corpus do
                let assemblyName = if disabled then "ByrefLikeOff" else "ByrefLikeOn"
                let ucos = (context.LoadFromStream (new MemoryStream (image))).GetType "Ucos"

                let dumped =
                    Assembly.read loggerFactory (Some $"%s{assemblyName}.dll") (new MemoryStream (image))

                let mutable state = baseState.Force().WithLoadedAssembly dumped

                for structName in names do
                    let methodName = "Direct" + structName
                    let hostRequired = refused (ucos.GetMethod methodName)

                    let parameter =
                        dumped.Methods.Values
                        |> Seq.filter (fun m -> m.Name = methodName)
                        |> Seq.exactlyOne
                        |> fun m -> List.exactlyOne m.Signature.ParameterTypes

                    let next, handle =
                        TypeSystemState.concretizeType
                            loggerFactory
                            HostImage.runtimeDirs
                            bct
                            state
                            dumped.DefinitionFullName
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            parameter
                        |> BindingResult.bound

                    let next, facts =
                        LayoutFacts.ofValueType loggerFactory HostImage.runtimeDirs bct next handle
                        |> BindingResult.bound

                    let next, holdsReferences =
                        GcReferences.containsReferences loggerFactory HostImage.runtimeDirs bct next handle
                        |> BindingResult.bound

                    state <- next

                    let facts =
                        facts
                        |> Option.defaultWith (fun () -> failwith $"%s{structName} is not a value type to LayoutFacts")

                    if marshalingRequired (not disabled) false facts <> hostRequired then
                        failwith
                            $"%s{structName}, runtime marshalling disabled %b{disabled}: host refuses %b{hostRequired}, facts %A{facts}"

                    // With runtime marshalling off, CoreCLR compiles every one: none counts as holding
                    // GC pointers. With it on, a byref field is not blittable.
                    hostRequired |> shouldEqual (not disabled)
                    facts.ContainsGcPointers |> shouldEqual false
                    holdsReferences |> shouldEqual true
        finally
            context.Unload ()
