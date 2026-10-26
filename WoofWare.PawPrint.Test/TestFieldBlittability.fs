namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The per-field blittability table against CoreCLR, over sequential structs with one field.
/// A pinnable, non-generic struct is blittable exactly when marshalling accepts it and its
/// cleanup leaves a dirty buffer unchanged. Reference-containing structs are not blittable;
/// their cleanup must not be called on a dirty reference. Byref-like structs instead use
/// CoreCLR's acceptance of an UnmanagedCallersOnly method with runtime marshalling enabled.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFieldBlittability =

    let private attribute (ty : Type) : CustomAttributeBuilder =
        CustomAttributeBuilder (ty.GetConstructor Type.EmptyTypes, [||])

    let private functionPointerType : Lazy<Type> =
        lazy
            (let image =
                Roslyn.compileAssembly
                    "FieldBlittabilityFunctionPointer"
                    Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                    []
                    [ "public unsafe struct Holder { public delegate*<void> F; }" ]

             (System.Reflection.Assembly.Load image).GetType("Holder").GetField("F").FieldType)

    type private Case =
        {
            Name : string
            Element : FieldBlittability.Element
            Ansi : bool
            Blob : byte[] option
            Field : FieldBuilder
        }

    /// Each possible leading byte, plus absent, empty, complete and truncated descriptors.
    let private blobs : byte[] option list =
        [
            None
            Some [||]
            for nativeType in 0..255 do
                Some [| byte nativeType |]
            // Complete fixed string and fixed array, with and without an element type.
            Some [| 0x17uy ; 0x02uy |]
            Some [| 0x1Euy ; 0x02uy |]
            Some [| 0x1Euy ; 0x02uy ; 0x07uy |]
            // Truncated size, and a complete custom-marshaler blob (four empty strings).
            Some [| 0x1Euy ; 0x80uy |]
            Some [| 0x2Cuy ; 0uy ; 0uy ; 0uy ; 0uy |]
            // Unread trailing bytes do not change a primitive descriptor.
            Some [| 0x07uy ; 0xFFuy ; 0xFFuy |]
        ]

    let private emit () : byte[] * Case list =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "FieldBlittability", typeof<obj>.Assembly)

        let md = builder.DefineDynamicModule "FieldBlittability"

        let simpleStruct (name : string) (layout : TypeAttributes) (field : Type) : Type =
            let tb =
                md.DefineType (name, TypeAttributes.Public ||| TypeAttributes.Sealed ||| layout, typeof<ValueType>)

            tb.DefineField ("X", field, FieldAttributes.Public) |> ignore
            tb.CreateType ()

        let intStruct = simpleStruct "IntStruct" TypeAttributes.SequentialLayout typeof<int>

        let boolStruct =
            simpleStruct "BoolStruct" TypeAttributes.SequentialLayout typeof<bool>

        let autoStruct = simpleStruct "AutoStruct" TypeAttributes.AutoLayout typeof<int>

        // Classifications are fixture inputs, not a type resolver. In particular, Decimal is
        // itself blittable but cannot be blitted as a field because its native alignment differs.
        let fields : (Type * FieldBlittability.Element) list =
            [
                typeof<bool>, FieldBlittability.Element.Boolean
                typeof<char>, FieldBlittability.Element.Char
                typeof<sbyte>, FieldBlittability.Element.OneByte
                typeof<byte>, FieldBlittability.Element.OneByte
                typeof<int16>, FieldBlittability.Element.TwoBytes
                typeof<uint16>, FieldBlittability.Element.TwoBytes
                typeof<int>, FieldBlittability.Element.FourBytes
                typeof<uint32>, FieldBlittability.Element.FourBytes
                typeof<int64>, FieldBlittability.Element.EightBytes
                typeof<uint64>, FieldBlittability.Element.EightBytes
                typeof<float32>, FieldBlittability.Element.Single
                typeof<float>, FieldBlittability.Element.Double
                typeof<nativeint>, FieldBlittability.Element.NativeInteger
                typeof<unativeint>, FieldBlittability.Element.NativeInteger
                typeof<int>.MakePointerType (), FieldBlittability.Element.Pointer
                functionPointerType.Force (), FieldBlittability.Element.FunctionPointer
                typeof<decimal>, FieldBlittability.Element.ValueType (true, true)
                intStruct, FieldBlittability.Element.ValueType (false, true)
                boolStruct, FieldBlittability.Element.ValueType (false, false)
                autoStruct, FieldBlittability.Element.ValueType (false, false)
                typeof<string>, FieldBlittability.Element.Reference
                typeof<obj>, FieldBlittability.Element.Reference
                typeof<int[]>, FieldBlittability.Element.Reference
                typeof<int>.MakeByRefType (), FieldBlittability.Element.Byref
                typeof<string>.MakeByRefType (), FieldBlittability.Element.Byref
                typeof<obj>.Assembly.GetType "System.TypedReference", FieldBlittability.Element.TypedReference
            ]

        let ucos =
            md.DefineType ("Ucos", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let cases =
            [
                for fieldType, element in fields do
                    for charset in
                        [
                            TypeAttributes.AnsiClass
                            TypeAttributes.UnicodeClass
                            TypeAttributes.AutoClass
                        ] do
                        for blob in blobs do
                            fieldType, element, charset, blob
            ]
            |> List.mapi (fun i (fieldType, element, charset, blob) ->
                let name = $"S%d{i}"

                let tb =
                    md.DefineType (
                        name,
                        TypeAttributes.Public
                        ||| TypeAttributes.Sealed
                        ||| TypeAttributes.SequentialLayout
                        ||| charset,
                        typeof<ValueType>
                    )

                if
                    element = FieldBlittability.Element.Byref
                    || element = FieldBlittability.Element.TypedReference
                then
                    tb.SetCustomAttribute (attribute typeof<IsByRefLikeAttribute>)

                let field =
                    tb.DefineField (
                        "X",
                        fieldType,
                        FieldAttributes.Public
                        ||| (if blob.IsSome then
                                 FieldAttributes.HasFieldMarshal
                             else
                                 enum 0)
                    )

                let t = tb.CreateType ()

                let mb =
                    ucos.DefineMethod (
                        name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<Void>,
                        [| t |]
                    )

                mb.SetCustomAttribute (attribute typeof<UnmanagedCallersOnlyAttribute>)
                mb.GetILGenerator().Emit OpCodes.Ret

                {
                    Name = name
                    Element = element
                    Ansi = charset <> TypeAttributes.UnicodeClass
                    Blob = blob
                    Field = field
                }
            )

        ucos.CreateType () |> ignore
        let metadata, il, fieldData = builder.GenerateMetadata ()

        for case in cases do
            match case.Blob with
            | None -> ()
            | Some blob ->
                metadata.AddMarshallingDescriptor (
                    MetadataTokens.EntityHandle case.Field.MetadataToken,
                    metadata.GetOrAddBlob blob
                )

        let pe =
            ManagedPEBuilder (
                PEHeaderBuilder.CreateLibraryHeader (),
                MetadataRootBuilder metadata,
                il,
                mappedFieldData = fieldData
            )

        let image = BlobBuilder ()
        pe.Serialize image |> ignore
        image.ToArray (), cases

    let private pinnable (t : Type) : bool =
        let boxed = RuntimeHelpers.GetUninitializedObject t

        try
            let handle = GCHandle.Alloc (boxed, GCHandleType.Pinned)
            handle.Free ()
            true
        with :? ArgumentException ->
            false

    let private hostBlittable (t : Type) (uco : System.Reflection.MethodInfo) : bool =
        if t.IsByRefLike then
            // These fields carry neither Int128 nor decimal floating point, so the UCO rule
            // with runtime marshalling enabled is exactly the containing struct's blittability.
            try
                RuntimeHelpers.PrepareMethod uco.MethodHandle
                true
            with :? InvalidProgramException ->
                false
        elif not (pinnable t) then
            false
        else
            let size =
                try
                    Some (Marshal.SizeOf t)
                with :? ArgumentException ->
                    None

            match size with
            | None -> false
            | Some size ->
                let dirty = 0xABuy
                let buffer = Marshal.AllocHGlobal size

                try
                    for i in 0 .. size - 1 do
                        Marshal.WriteByte (buffer, i, dirty)

                    // Only non-blittable types have a cleanup stub. It zeroes the image or
                    // refuses the field's marshalling descriptor.
                    try
                        Marshal.DestroyStructure (buffer, t)
                        [ 0 .. size - 1 ] |> List.forall (fun i -> Marshal.ReadByte (buffer, i) = dirty)
                    with
                    | :? ArgumentException
                    | :? MarshalDirectiveException -> false
                finally
                    Marshal.FreeHGlobal buffer

    [<Test>]
    let ``Every field kind and native-type byte agrees with CoreCLR`` () : unit =
        let image, cases = emit ()
        use pe = new PEReader (ImmutableArray.CreateRange image)
        let metadata = pe.GetMetadataReader ()

        metadata.GetTableRowCount TableIndex.FieldMarshal
        |> shouldEqual (cases |> List.filter (fun c -> c.Blob.IsSome) |> List.length)

        let context =
            Runtime.Loader.AssemblyLoadContext ("FieldBlittability", isCollectible = true)

        try
            use stream = new MemoryStream (image)
            let assembly = context.LoadFromStream stream
            let ucos = assembly.GetType "Ucos"
            let mutable accepted = 0
            let mutable refused = 0

            for case in cases do
                let field =
                    metadata.GetFieldDefinition (MetadataTokens.FieldDefinitionHandle case.Field.MetadataToken)

                let actualBlob = metadata.GetBlobContent (field.GetMarshallingDescriptor ())
                actualBlob |> Seq.toArray |> shouldEqual (case.Blob |> Option.defaultValue [||])

                let descriptor =
                    case.Blob |> Option.bind (fun _ -> FieldMarshalDescriptor.ofBlob actualBlob)

                let host = hostBlittable (assembly.GetType case.Name) (ucos.GetMethod case.Name)
                let actual = FieldBlittability.isBlittable case.Ansi descriptor case.Element

                if host then
                    accepted <- accepted + 1
                else
                    refused <- refused + 1

                if actual <> host then
                    failwith
                        $"%s{case.Name}: %A{case.Element}, ANSI=%b{case.Ansi}, blob=%A{case.Blob}: host=%b{host}, table=%b{actual}"

            accepted |> shouldBeGreaterThan 0
            refused |> shouldBeGreaterThan 0
            TestContext.WriteLine $"Checked %d{cases.Length} cases: %d{accepted} blittable, %d{refused} non-blittable."
        finally
            context.Unload ()
