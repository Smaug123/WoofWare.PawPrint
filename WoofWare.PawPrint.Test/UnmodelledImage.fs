namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open WoofWare.PawPrint

/// A hand-emitted image whose metadata names what PawPrint cannot bind, in each of the shapes the
/// binding-failure tests ask about, read beside the host's CoreLib.
[<RequireQualifiedAccess>]
module internal UnmodelledImage =

    /// The handles of an emitted image's shapes.
    type Shapes =
        {
            /// `System.Gone`, scoped to CoreLib, which declares no such type.
            Absent : TypeReferenceHandle
            /// `System.Gone`, scoped to an assembly named `Nowhere`.
            Unbound : TypeReferenceHandle
            /// `System.Collections.Generic.List`1`, scoped to CoreLib.
            List : TypeReferenceHandle
            /// `N.Inner`, scoped to a module reference.
            ModuleScoped : TypeReferenceHandle
            /// A field reference whose parent is a module reference.
            ModuleParent : MemberReferenceHandle
            /// A field reference whose parent is a method definition.
            MethodParent : MemberReferenceHandle
            /// A reference to `Object.ToString`, a method, read as a field.
            MethodSignature : MemberReferenceHandle
            /// `N.Exported`, exported from another file of the assembly.
            FileExport : ExportedTypeHandle
            /// `N.Derived`, whose base type is `System.Gone`, which CoreLib does not declare.
            Derived : TypeDefinitionHandle
            /// `System.Object`, scoped to CoreLib.
            Object : TypeReferenceHandle
            /// `System.Object`, scoped to an assembly named `Nowhere`.
            UnboundObject : TypeReferenceHandle
            /// `static void N.Holder.Takes(class System.Gone)`, naming the absent type.
            Takes : MethodDefinitionHandle
            /// `void N.Derived.Run()`, declared on the type whose base type is absent.
            DerivedRun : MethodDefinitionHandle
            /// `N.Derived::Run()`, whose parent's base type is absent.
            DerivedRunReference : MemberReferenceHandle
            /// A method reference whose parent is a module reference.
            ModuleMethod : MemberReferenceHandle
            /// `System.Object::NoSuch()`, which CoreLib does not declare.
            NoSuchMethod : MemberReferenceHandle
            /// `System.Gone::M()`, whose parent CoreLib does not declare.
            AbsentParentMethod : MemberReferenceHandle
            /// `int32 [Nowhere]System.Gone::F`, whose parent is in an assembly no runtime directory
            /// supplies.
            UnboundParentField : MemberReferenceHandle
            /// `System.Gone`, scoped to `System.Runtime`, which neither declares nor forwards it, so
            /// that resolving it loads `System.Runtime` before it fails.
            FacadeAbsent : TypeReferenceHandle
            /// `System.Collections.Generic.List`1<int32>`.
            InstantiatedList : TypeSpecificationHandle
            /// `[Nowhere]System.Gone<int32>`.
            UnboundInstantiation : TypeSpecificationHandle
            /// `int32 N.Derived::Count`, declared on the type whose base type is absent.
            DerivedCount : FieldDefinitionHandle
            /// `List`1::Clear()`, through the generic definition with no instantiation.
            OpenListClear : MemberReferenceHandle
            /// `N.Facaded`, whose base type is `System.Object` named through `System.Runtime`, so
            /// that resolving its base chain loads `System.Runtime`.
            Facaded : TypeDefinitionHandle
            /// `[System.Runtime]System.Nullable`1<int32>`.
            FacadeNullable : TypeSpecificationHandle
            /// `[System.Runtime]System.Object::ToString()`.
            FacadeToString : MemberReferenceHandle
            /// `GENERICINST class System.Collections.Generic.List`1<class System.Collections.Generic.List`1>`,
            /// whose argument names the generic definition without instantiating it.
            ListOfBareList : TypeSpecificationHandle
            /// `List`1<List`1>::Clear()`, whose parent is <c>ListOfBareList</c>.
            ListOfBareListClear : MemberReferenceHandle
            /// `class System.Collections.Generic.List`1`: a signature that is the generic definition
            /// alone, with no instantiation.
            BareList : TypeSpecificationHandle
            /// `List`1::Clear()`, whose parent is <c>BareList</c>.
            BareListClear : MemberReferenceHandle
            /// `!0`, the first type parameter of whatever environment reads it.
            TypeParameter : TypeSpecificationHandle
            /// `!!0`, the first method parameter of whatever environment reads it.
            MethodParameter : TypeSpecificationHandle
            /// `System.Collections.Generic.List`1<!0>`.
            ListOfTypeParameter : TypeSpecificationHandle
            /// `List`1<!0>::Clear()`, whose parent is <c>ListOfTypeParameter</c>.
            ListOfTypeParameterClear : MemberReferenceHandle
            /// `System.Collections.Generic.List`1<!!0>`.
            ListOfMethodParameter : TypeSpecificationHandle
            /// `List`1<!!0>::Clear()`, whose parent is <c>ListOfMethodParameter</c>.
            ListOfMethodParameterClear : MemberReferenceHandle
            /// `class System.Gone[]`, whose element type CoreLib does not declare.
            AbsentVector : TypeSpecificationHandle
            /// `class [Nowhere]System.Gone[]`, whose element type is in an assembly no runtime
            /// directory supplies.
            UnboundVector : TypeSpecificationHandle
            /// `!0[]`.
            TypeParameterVector : TypeSpecificationHandle
            /// `class System.Gone[0...,0...]`, whose element type CoreLib does not declare.
            AbsentMatrix : TypeSpecificationHandle
            /// `System.Gone[]::ToString()`, whose parent is <c>AbsentVector</c>.
            AbsentVectorToString : MemberReferenceHandle
            /// `!0[]::ToString()`, whose parent is <c>TypeParameterVector</c>.
            TypeParameterVectorToString : MemberReferenceHandle
            /// `System.Gone[,]::Get(int32, int32)`, whose parent is <c>AbsentMatrix</c>.
            AbsentMatrixGet : MemberReferenceHandle
        }

    let emit () : byte[] * Shapes =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Unmodelled.dll",
            metadata.GetOrAddGuid (Guid "0d4b9e61-2a7c-4e3f-9b8d-5c6e7f8a9b0c"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Unmodelled",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let nowhereRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString "Nowhere",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let runtimeName = System.Reflection.Assembly.Load("System.Runtime").GetName ()

        let runtimeRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString runtimeName.Name,
                runtimeName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (runtimeName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (scope : EntityHandle) (ns : string) (name : string) : TypeReferenceHandle =
            metadata.AddTypeReference (scope, metadata.GetOrAddString ns, metadata.GetOrAddString name)

        let objectRef =
            typeRef (AssemblyReferenceHandle.op_Implicit corelibRef) "System" "Object"

        let absent =
            typeRef (AssemblyReferenceHandle.op_Implicit corelibRef) "System" "Gone"

        let unbound =
            typeRef (AssemblyReferenceHandle.op_Implicit nowhereRef) "System" "Gone"

        let unboundObject =
            typeRef (AssemblyReferenceHandle.op_Implicit nowhereRef) "System" "Object"

        let list =
            typeRef (AssemblyReferenceHandle.op_Implicit corelibRef) "System.Collections.Generic" "List`1"

        let facadeAbsent =
            typeRef (AssemblyReferenceHandle.op_Implicit runtimeRef) "System" "Gone"

        let instantiation (generic : TypeReferenceHandle) : TypeSpecificationHandle =
            let blob = BlobBuilder ()

            let arguments =
                BlobEncoder(blob)
                    .TypeSpecificationSignature()
                    .GenericInstantiation (TypeReferenceHandle.op_Implicit generic, 1, false)

            arguments.AddArgument().Int32 ()
            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)

        let facadeObject =
            typeRef (AssemblyReferenceHandle.op_Implicit runtimeRef) "System" "Object"

        let facadeNullable =
            instantiation (typeRef (AssemblyReferenceHandle.op_Implicit runtimeRef) "System" "Nullable`1")

        let instantiatedList = instantiation list
        let unboundInstantiation = instantiation unbound

        let otherModule =
            metadata.AddModuleReference (metadata.GetOrAddString "Other.netmodule")

        let moduleScoped =
            typeRef (ModuleReferenceHandle.op_Implicit otherModule) "N" "Inner"

        let int32Field =
            let blob = BlobBuilder ()
            BlobEncoder(blob).Field().Type().Int32 ()
            metadata.GetOrAddBlob blob

        let toStringSignature =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Type().String ()), (fun _ -> ()))

            metadata.GetOrAddBlob blob

        let voidSignature =
            let blob = BlobBuilder ()

            BlobEncoder(blob).MethodSignature().Parameters (0, (fun returnType -> returnType.Void ()), (fun _ -> ()))

            metadata.GetOrAddBlob blob

        let instanceVoidSignature =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Void ()), (fun _ -> ()))

            metadata.GetOrAddBlob blob

        let takesSignature =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters ->
                        parameters.AddParameter().Type().Type (TypeReferenceHandle.op_Implicit absent, false)
                    )
                )

            metadata.GetOrAddBlob blob

        let body =
            let il = InstructionEncoder (BlobBuilder ())
            il.OpCode ILOpCode.Ret
            MethodBodyStreamEncoder(ilStream).AddMethodBody il

        // `<Module>` is row 1, `Holder` row 2, which declares `M` and `Takes`, MethodDef rows 1 and
        // 2, `Derived` row 3, which declares `Run`, MethodDef row 3, and `Count`, FieldDef row 1, and
        // `Facaded` row 4, which declares nothing.
        metadata.AddTypeDefinition (
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "N",
            metadata.GetOrAddString "Holder",
            TypeReferenceHandle.op_Implicit objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let derived =
            metadata.AddTypeDefinition (
                TypeAttributes.Public,
                metadata.GetOrAddString "N",
                metadata.GetOrAddString "Derived",
                TypeReferenceHandle.op_Implicit absent,
                MetadataTokens.FieldDefinitionHandle 1,
                MetadataTokens.MethodDefinitionHandle 3
            )

        let facaded =
            metadata.AddTypeDefinition (
                TypeAttributes.Public,
                metadata.GetOrAddString "N",
                metadata.GetOrAddString "Facaded",
                TypeReferenceHandle.op_Implicit facadeObject,
                MetadataTokens.FieldDefinitionHandle 2,
                MetadataTokens.MethodDefinitionHandle 4
            )

        let derivedCount =
            metadata.AddFieldDefinition (FieldAttributes.Public, metadata.GetOrAddString "Count", int32Field)

        let m =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "M",
                voidSignature,
                body,
                MetadataTokens.ParameterHandle 1
            )

        let takes =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "Takes",
                takesSignature,
                body,
                MetadataTokens.ParameterHandle 1
            )

        let derivedRun =
            metadata.AddMethodDefinition (
                MethodAttributes.Public,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "Run",
                instanceVoidSignature,
                body,
                MetadataTokens.ParameterHandle 1
            )

        let derivedRunReference =
            metadata.AddMemberReference (
                TypeDefinitionHandle.op_Implicit derived,
                metadata.GetOrAddString "Run",
                instanceVoidSignature
            )

        let moduleMethod =
            metadata.AddMemberReference (
                ModuleReferenceHandle.op_Implicit otherModule,
                metadata.GetOrAddString "G",
                voidSignature
            )

        let noSuchMethod =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit objectRef,
                metadata.GetOrAddString "NoSuch",
                instanceVoidSignature
            )

        let absentParentMethod =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit absent,
                metadata.GetOrAddString "M",
                instanceVoidSignature
            )

        let unboundParentField =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit unbound,
                metadata.GetOrAddString "F",
                int32Field
            )

        let listOfBareList =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .TypeSpecificationSignature()
                .GenericInstantiation(TypeReferenceHandle.op_Implicit list, 1, false)
                .AddArgument()
                .Type (TypeReferenceHandle.op_Implicit list, false)

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)

        let listOfBareListClear =
            metadata.AddMemberReference (
                TypeSpecificationHandle.op_Implicit listOfBareList,
                metadata.GetOrAddString "Clear",
                instanceVoidSignature
            )

        let parameterSpec (encode : SignatureTypeEncoder -> unit) : TypeSpecificationHandle =
            let blob = BlobBuilder ()
            BlobEncoder(blob).TypeSpecificationSignature () |> encode
            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)

        let bareList =
            parameterSpec (fun spec -> spec.Type (TypeReferenceHandle.op_Implicit list, false))

        let bareListClear =
            metadata.AddMemberReference (
                TypeSpecificationHandle.op_Implicit bareList,
                metadata.GetOrAddString "Clear",
                instanceVoidSignature
            )

        let listOfSpec (encode : SignatureTypeEncoder -> unit) : TypeSpecificationHandle =
            parameterSpec (fun spec ->
                spec.GenericInstantiation(TypeReferenceHandle.op_Implicit list, 1, false).AddArgument ()
                |> encode
            )

        let clearOn (parent : TypeSpecificationHandle) : MemberReferenceHandle =
            metadata.AddMemberReference (
                TypeSpecificationHandle.op_Implicit parent,
                metadata.GetOrAddString "Clear",
                instanceVoidSignature
            )

        let typeParameter = parameterSpec (fun spec -> spec.GenericTypeParameter 0)
        let methodParameter = parameterSpec (fun spec -> spec.GenericMethodTypeParameter 0)
        let listOfTypeParameter = listOfSpec (fun arg -> arg.GenericTypeParameter 0)
        let listOfTypeParameterClear = clearOn listOfTypeParameter
        let listOfMethodParameter = listOfSpec (fun arg -> arg.GenericMethodTypeParameter 0)
        let listOfMethodParameterClear = clearOn listOfMethodParameter

        let absentVector =
            parameterSpec (fun spec -> spec.SZArray().Type (TypeReferenceHandle.op_Implicit absent, false))

        let unboundVector =
            parameterSpec (fun spec -> spec.SZArray().Type (TypeReferenceHandle.op_Implicit unbound, false))

        let typeParameterVector =
            parameterSpec (fun spec -> spec.SZArray().GenericTypeParameter 0)

        let absentMatrix =
            parameterSpec (fun spec ->
                spec.Array (
                    (fun element -> element.Type (TypeReferenceHandle.op_Implicit absent, false)),
                    (fun shape -> shape.Shape (2, ImmutableArray.Empty, ImmutableArray.Create (0, 0)))
                )
            )

        let toStringOn (parent : TypeSpecificationHandle) : MemberReferenceHandle =
            metadata.AddMemberReference (
                TypeSpecificationHandle.op_Implicit parent,
                metadata.GetOrAddString "ToString",
                toStringSignature
            )

        let absentVectorToString = toStringOn absentVector
        let typeParameterVectorToString = toStringOn typeParameterVector

        let absentMatrixGet =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    2,
                    (fun returnType -> returnType.Type().Type (TypeReferenceHandle.op_Implicit absent, false)),
                    (fun parameters ->
                        parameters.AddParameter().Type().Int32 ()
                        parameters.AddParameter().Type().Int32 ()
                    )
                )

            metadata.AddMemberReference (
                TypeSpecificationHandle.op_Implicit absentMatrix,
                metadata.GetOrAddString "Get",
                metadata.GetOrAddBlob blob
            )

        let moduleParent =
            metadata.AddMemberReference (
                ModuleReferenceHandle.op_Implicit otherModule,
                metadata.GetOrAddString "F",
                int32Field
            )

        let methodParent =
            metadata.AddMemberReference (MethodDefinitionHandle.op_Implicit m, metadata.GetOrAddString "F", int32Field)

        let openListClear =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit list,
                metadata.GetOrAddString "Clear",
                instanceVoidSignature
            )

        let facadeToString =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit facadeObject,
                metadata.GetOrAddString "ToString",
                toStringSignature
            )

        let methodSignature =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit objectRef,
                metadata.GetOrAddString "ToString",
                toStringSignature
            )

        let file =
            metadata.AddAssemblyFile (
                metadata.GetOrAddString "Other.netmodule",
                metadata.GetOrAddBlob [| 0uy |],
                containsMetadata = true
            )

        let fileExport =
            metadata.AddExportedType (
                TypeAttributes.Public,
                metadata.GetOrAddString "N",
                metadata.GetOrAddString "Exported",
                AssemblyFileHandle.op_Implicit file,
                0
            )

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        image.ToArray (),
        {
            Absent = absent
            Unbound = unbound
            List = list
            ModuleScoped = moduleScoped
            ModuleParent = moduleParent
            MethodParent = methodParent
            MethodSignature = methodSignature
            FileExport = fileExport
            Derived = derived
            Object = objectRef
            UnboundObject = unboundObject
            Takes = takes
            DerivedRun = derivedRun
            DerivedRunReference = derivedRunReference
            ModuleMethod = moduleMethod
            NoSuchMethod = noSuchMethod
            AbsentParentMethod = absentParentMethod
            UnboundParentField = unboundParentField
            FacadeAbsent = facadeAbsent
            InstantiatedList = instantiatedList
            UnboundInstantiation = unboundInstantiation
            DerivedCount = derivedCount
            OpenListClear = openListClear
            Facaded = facaded
            FacadeNullable = facadeNullable
            FacadeToString = facadeToString
            ListOfBareList = listOfBareList
            ListOfBareListClear = listOfBareListClear
            BareList = bareList
            BareListClear = bareListClear
            TypeParameter = typeParameter
            MethodParameter = methodParameter
            ListOfTypeParameter = listOfTypeParameter
            ListOfTypeParameterClear = listOfTypeParameterClear
            ListOfMethodParameter = listOfMethodParameter
            ListOfMethodParameterClear = listOfMethodParameterClear
            AbsentVector = absentVector
            UnboundVector = unboundVector
            TypeParameterVector = typeParameterVector
            AbsentMatrix = absentMatrix
            AbsentVectorToString = absentVectorToString
            TypeParameterVectorToString = typeParameterVectorToString
            AbsentMatrixGet = absentMatrixGet
        }

    /// The emitted image read, CoreLib, and everything needed to resolve against them.
    type Fixture =
        {
            LoggerFactory : Microsoft.Extensions.Logging.ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assemblies : LoadedAssemblies
            Analysed : DumpedAssembly
            Shapes : Shapes
        }

    let fixture () : Fixture =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (HostImage.frameworkDirectory, "System.Private.CoreLib.dll"))

        let image, shapes = emit ()

        let analysed =
            Assembly.read loggerFactory (Some "Unmodelled.dll") (new MemoryStream (image))

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = BaseClassTypes.ofCorelib corelib
            Assemblies = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]
            Analysed = analysed
            Shapes = shapes
        }
