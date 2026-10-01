namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open NUnit.Framework

/// A `call` whose MemberRef names a method through a type that inherits it, against the real
/// runtime.
///
/// C# always names the declaring type, so the reference here is fabricated: `call instance int32
/// Derived<A, B>::Check()`, where `Derived<A, B> : Base<B>` and only `Base<T>` declares `Check`.
/// CoreCLR finds the method by searching `Derived`'s method table and then its base class
/// (`MemberLoader::FindMethod`), and runs it on the instantiation of `Base` that `Derived`'s
/// extends clause makes of the reference's arguments: `Base<long>` for `Derived<int, long>`.
/// `Check` reports whether its `T` is `long`, so running it on the parent's arguments instead
/// (`Base<int>`) is observable.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedInheritedMemberReference =

    /// Re-point the `parent` column of every MemberRef named `name` whose parent is one of the
    /// TypeSpecs `from` at the TypeSpec `onto`, leaving the rest of the image as it was.
    let private repointMemberReferences
        (image : byte[])
        (name : string)
        (from : TypeSpecificationHandle list)
        (onto : TypeSpecificationHandle)
        : byte[]
        =
        use pe = new PEReader (new MemoryStream (image))
        let reader = pe.GetMetadataReader ()
        let metadataStart = pe.PEHeaders.MetadataStartOffset
        let tableStart = reader.GetTableMetadataOffset TableIndex.MemberRef
        let rowSize = reader.GetTableRowSize TableIndex.MemberRef

        // MemberRefParent is a coded index over five tables, so its tag is three bits wide
        // (ECMA-335 II.24.2.6), and TypeSpec's tag is 4.
        let coded (handle : TypeSpecificationHandle) : int =
            (MetadataTokens.GetRowNumber (TypeSpecificationHandle.op_Implicit handle : EntityHandle)
             <<< 3)
            ||| 4

        let largestRow =
            [
                TableIndex.TypeDef
                TableIndex.TypeRef
                TableIndex.ModuleRef
                TableIndex.MethodDef
                TableIndex.TypeSpec
            ]
            |> List.map reader.GetTableRowCount
            |> List.max

        if largestRow >= (1 <<< 13) then
            failwith "the fabricated image is too large for a two-byte MemberRefParent column"

        let patched = Array.copy image
        let mutable repointed = 0

        for handle in reader.MemberReferences do
            let reference = reader.GetMemberReference handle

            if
                reader.GetString reference.Name = name
                && from
                   |> List.exists (fun from -> reference.Parent = TypeSpecificationHandle.op_Implicit from)
            then
                let row =
                    MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let offset = metadataStart + tableStart + (row - 1) * rowSize
                let value = coded onto
                patched.[offset] <- byte (value &&& 0xFF)
                patched.[offset + 1] <- byte (value >>> 8)
                repointed <- repointed + 1

        if repointed = 0 then
            failwith $"no MemberRef named %s{name} has the parent to re-point"

        patched

    /// `Base<T>` declaring `int Check()`; `Derived<A, B> : Base<B>` and `Swap<A> : Base<string>`, each
    /// constructible; and `Fab.Run()`, which returns `100 * new Derived<int, long>().Check() + 10 *
    /// new Derived<long, int>().Check() + new Swap<int>().Check()`, with each `Check` named through
    /// its receiver's instantiation. `Swap` has as many type parameters as `Base`, so running its
    /// `Check` on the parent's arguments gives a wrong answer rather than an arity mismatch.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Inherited", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Inherited"

        let baseType =
            modul.DefineType ("Base", TypeAttributes.Public ||| TypeAttributes.Class)

        let t = (baseType.DefineGenericParameters [| "T" |]).[0]
        let baseCtor = baseType.DefineDefaultConstructor MethodAttributes.Public

        // 1 if `T` is `long`, 2 if it is `int`, otherwise 3.
        let check =
            baseType.DefineMethod ("Check", MethodAttributes.Public ||| MethodAttributes.HideBySig, typeof<int>, [||])

        do
            let il = check.GetILGenerator ()
            let getTypeFromHandle = typeof<Type>.GetMethod (nameof Type.GetTypeFromHandle)

            let opEquality =
                typeof<Type>.GetMethod ("op_Equality", [| typeof<Type> ; typeof<Type> |])

            for candidate, answer in [ typeof<int64>, 1 ; typeof<int>, 2 ] do
                let next = il.DefineLabel ()
                il.Emit (OpCodes.Ldtoken, t)
                il.Emit (OpCodes.Call, getTypeFromHandle)
                il.Emit (OpCodes.Ldtoken, candidate)
                il.Emit (OpCodes.Call, getTypeFromHandle)
                il.Emit (OpCodes.Call, opEquality)
                il.Emit (OpCodes.Brfalse_S, next)
                il.Emit (OpCodes.Ldc_I4, answer)
                il.Emit OpCodes.Ret
                il.MarkLabel next

            il.Emit OpCodes.Ldc_I4_3
            il.Emit OpCodes.Ret

        baseType.CreateType () |> ignore<Type>

        /// A generic class `name` extending `Base<baseArgument parameters>`, with a constructor.
        let defineDerived
            (name : string)
            (parameterNames : string[])
            (baseArgument : Type[] -> Type)
            : TypeBuilder * ConstructorBuilder
            =
            let derived =
                modul.DefineType (name, TypeAttributes.Public ||| TypeAttributes.Class)

            let parameters =
                derived.DefineGenericParameters parameterNames |> Array.map (fun p -> p :> Type)

            let parent = baseType.MakeGenericType [| baseArgument parameters |]
            derived.SetParent parent

            let ctor =
                derived.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [||])

            let il = ctor.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Call, TypeBuilder.GetConstructor (parent, baseCtor))
            il.Emit OpCodes.Ret

            derived.CreateType () |> ignore<Type>
            derived, ctor

        let derived =
            defineDerived "Derived" [| "A" ; "B" |] (fun parameters -> parameters.[1])

        let swap = defineDerived "Swap" [| "A" |] (fun _ -> typeof<string>)

        let fab =
            modul.DefineType ("Fab", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let run =
            fab.DefineMethod ("Run", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])

        // Each call names `Base<X>::Check` for the `X` its receiver's extends clause supplies, and
        // the patch below re-points it at the receiver's own instantiation. No two receivers share
        // an `X`, because a MemberRef row is shared by every call site naming the same member.
        let calls =
            [
                "Derived", derived, [ typeof<int> ; typeof<int64> ], typeof<int64>
                "Derived", derived, [ typeof<int64> ; typeof<int> ], typeof<int>
                "Swap", swap, [ typeof<int> ], typeof<string>
            ]

        do
            let il = run.GetILGenerator ()

            for index, (_, (receiver, ctor), arguments, baseArgument) in List.indexed calls do
                if index > 0 then
                    il.Emit (OpCodes.Ldc_I4_S, 10y)
                    il.Emit OpCodes.Mul

                il.Emit (
                    OpCodes.Newobj,
                    TypeBuilder.GetConstructor (receiver.MakeGenericType (Array.ofList arguments), ctor)
                )

                il.Emit (OpCodes.Call, TypeBuilder.GetMethod (baseType.MakeGenericType [| baseArgument |], check))

                if index > 0 then
                    il.Emit OpCodes.Add

            il.Emit OpCodes.Ret

        fab.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        let image = stream.ToArray ()

        // Every TypeSpec row spelling `definition<arguments>`. The builder writes a fresh row for
        // each use, so there can be several, and rows with equal blobs denote the same type.
        let typeSpecsOf (definition : string) (arguments : Type list) : TypeSpecificationHandle list =
            use pe = new PEReader (new MemoryStream (image))
            let reader = pe.GetMetadataReader ()

            let code (ty : Type) : SignatureTypeCode =
                if ty = typeof<int> then SignatureTypeCode.Int32
                elif ty = typeof<int64> then SignatureTypeCode.Int64
                elif ty = typeof<string> then SignatureTypeCode.String
                else failwith $"no fabricated instantiation uses %O{ty}"

            let matches (handle : TypeSpecificationHandle) : bool =
                let mutable blob =
                    reader.GetBlobReader (reader.GetTypeSpecification handle).Signature

                if blob.ReadSignatureTypeCode () <> SignatureTypeCode.GenericTypeInstance then
                    false
                else

                // CLASS, then the generic definition.
                blob.ReadSignatureTypeCode () |> ignore<SignatureTypeCode>
                let root = blob.ReadTypeHandle ()

                let isDefinition =
                    root.Kind = HandleKind.TypeDefinition
                    && reader.GetString (reader.GetTypeDefinition (TypeDefinitionHandle.op_Explicit root)).Name = definition

                if not isDefinition || blob.ReadCompressedInteger () <> arguments.Length then
                    false
                else

                let mutable agree = true

                for argument in arguments do
                    agree <- agree && blob.ReadSignatureTypeCode () = code argument

                agree

            seq { 1 .. reader.GetTableRowCount TableIndex.TypeSpec }
            |> Seq.map MetadataTokens.TypeSpecificationHandle
            |> Seq.filter matches
            |> List.ofSeq

        (image, calls)
        ||> List.fold (fun image (name, _, arguments, baseArgument) ->
            match typeSpecsOf name arguments with
            | onto :: _ -> repointMemberReferences image "Check" (typeSpecsOf "Base" [ baseArgument ]) onto
            | [] -> failwith $"no TypeSpec spells %s{name}%A{arguments}"
        )

    let private driverSource : string =
        """
public static class Driver
{
    public static int Main(string[] args) => Fab.Run() == 123 ? 0 : Fab.Run();
}
"""

    [<Test>]
    let ``a MemberRef naming a method through an inheriting type runs it on the ancestor's instantiation`` () : unit =
        FabricatedGuest.run "Inherited" (fabricate ()) "InheritedDriver" driverSource 0
