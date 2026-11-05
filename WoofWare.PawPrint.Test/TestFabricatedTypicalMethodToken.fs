namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open NUnit.Framework

/// A `call` or `ldftn` whose token names a method of a generic type by its MethodDef row, so
/// without saying at which instantiation, made from a method of that type running at
/// `Gen<string>`. No compiler emits these: C# names such a method through a MemberRef whose parent
/// is a TypeSpec spelling the instantiation out.
///
/// CoreCLR binds such a token to the typical instantiation, `Gen<T>`'s own method
/// (`MemberLoader::GetMethodDescFromMethodDef`), which nothing can run. Measured on .NET 10: a
/// `call` raises `InvalidOperationException` from the callee ("Could not execute the method because
/// either the method itself or the containing type is not fully instantiated."), and an `ldftn`
/// makes its pointer. PawPrint has no typical instantiation to bind to, so it refuses each, rather
/// than binding at the executing frame's instantiation, which is not what the token says.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedTypicalMethodToken =

    [<RequireQualifiedAccess>]
    type private Shape =
        /// `call int32 Gen`1::Answer()`, by MethodDef.
        | CallMethodDefinition
        /// `call int32 Gen`1::Echo<int32>()`, a MethodSpec over a MethodDef.
        | CallMethodSpecification
        /// `ldftn int32 Gen`1::Answer()`, by MethodDef.
        | LoadFunctionPointer

    /// `class Gen<T>` with `static int Answer()` returning 5, `static int Echo<U>()` returning 6, and
    /// `static int Caller()`, whose body is `shape`'s instruction followed by whatever returns an
    /// `int`.
    let private fabricate (shape : Shape) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Typical.dll",
            metadata.GetOrAddGuid (Guid "5b1d7c3e-8a24-4f96-b0e1-2c7d9a64f318"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Typical",
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

        let objectRef : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )
            |> TypeReferenceHandle.op_Implicit

        let staticSignature (genericParameters : int) (returns : ReturnTypeEncoder -> unit) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(genericParameterCount = genericParameters)
                .Parameters (0, returns, ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let returningInt = staticSignature 0 (fun returns -> returns.Type().Int32 ())

        let body (emit : InstructionEncoder -> unit) : int =
            let code = InstructionEncoder (BlobBuilder ())
            emit code
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let addMethod (name : string) (signature : BlobHandle) (bodyOffset : int) : MethodDefinitionHandle =
            metadata.AddMethodDefinition (
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                bodyOffset,
                Unchecked.defaultof<ParameterHandle>
            )

        // The two callees come first, so `Caller`'s body can name their rows.
        let answer =
            addMethod "Answer" returningInt (body (fun code -> code.LoadConstantI4 5))

        let echo =
            addMethod
                "Echo"
                (staticSignature 1 (fun returns -> returns.Type().Int32 ()))
                (body (fun code -> code.LoadConstantI4 6))

        let callerBody =
            body (fun code ->
                match shape with
                | Shape.CallMethodDefinition -> code.Call answer
                | Shape.CallMethodSpecification ->
                    let instantiation = BlobBuilder ()
                    BlobEncoder(instantiation).MethodSpecificationSignature(1).AddArgument().Int32 ()

                    let spec =
                        metadata.AddMethodSpecification (
                            (MethodDefinitionHandle.op_Implicit echo : EntityHandle),
                            metadata.GetOrAddBlob instantiation
                        )

                    code.Call (MethodSpecificationHandle.op_Implicit spec : EntityHandle)
                | Shape.LoadFunctionPointer ->
                    code.OpCode ILOpCode.Ldftn
                    code.Token (MethodDefinitionHandle.op_Implicit answer : EntityHandle)
                    code.OpCode ILOpCode.Pop
                    code.LoadConstantI4 5
            )

        addMethod "Caller" returningInt callerBody |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            answer
        )
        |> ignore<TypeDefinitionHandle>

        let gen =
            metadata.AddTypeDefinition (
                TypeAttributes.Public
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.BeforeFieldInit,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "Gen`1",
                objectRef,
                MetadataTokens.FieldDefinitionHandle 1,
                answer
            )

        metadata.AddGenericParameter (
            (TypeDefinitionHandle.op_Implicit gen : EntityHandle),
            GenericParameterAttributes.None,
            metadata.GetOrAddString "T",
            0
        )
        |> ignore<GenericParameterHandle>

        metadata.AddGenericParameter (
            (MethodDefinitionHandle.op_Implicit echo : EntityHandle),
            GenericParameterAttributes.None,
            metadata.GetOrAddString "U",
            0
        )
        |> ignore<GenericParameterHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                ilStream,
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    let private driver : string =
        """
public static class Driver
{
    public static int Main(string[] args) => Gen<string>.Caller();
}
"""

    let rec private messages (e : exn) : string list =
        match e.InnerException with
        | null -> [ e.Message ]
        | inner -> e.Message :: messages inner

    let private shapes : Map<string, Shape> =
        [
            Shape.CallMethodDefinition
            Shape.CallMethodSpecification
            Shape.LoadFunctionPointer
        ]
        |> List.map (fun shape -> string<Shape> shape, shape)
        |> Map.ofList

    let shapeNames : string list = shapes |> Map.keys |> List.ofSeq

    [<TestCaseSource(nameof shapeNames)>]
    let ``a MethodDef token naming a generic type's member is refused`` (name : string) : unit =
        let shape = shapes.[name]

        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "Typical" (fabricate shape) $"Typical%s{name}Driver" driver

        match shape, onHost with
        | Shape.LoadFunctionPointer, RealRuntimeResult.NormalExit 5 -> ()
        | (Shape.CallMethodDefinition | Shape.CallMethodSpecification), RealRuntimeResult.UnhandledException report when
            report.Contains
                "System.InvalidOperationException: Could not execute the method because either the method itself or the containing type is not fully instantiated."
            ->
            ()
        | _, other -> failwith $"real runtime: %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Exited code ->
            failwith $"PawPrint ran the guest to exit code %d{code}, binding the token at the frame's instantiation"
        | FabricatedOutcome.Failed e ->
            let messages = messages e

            if
                not (
                    messages
                    |> List.exists (fun message -> message.Contains "typical instantiation")
                )
            then
                failwith $"PawPrint did not refuse the token as naming the typical instantiation: %A{messages}"
