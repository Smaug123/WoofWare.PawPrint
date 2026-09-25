namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `VmSubstitution.unsafeStub` is a transcription of the IL CoreCLR's VM runs for each
/// `System.Runtime.CompilerServices.Unsafe` method corelib.h binds. These tests emit that IL, as
/// Semantics gives it, into a fabricated `Stubs` class with each method's own signature, and have
/// a driver compare every `Unsafe` method with its stub.
///
/// On the real runtime that checks the transcription: both sides are what CoreCLR runs. On
/// PawPrint it checks `Intrinsics.call` against the IL it stands in for, since PawPrint runs its
/// own implementation of a method in preference to the stub.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeStubs =

    let private fabricatedName = "UnsafeStubs"

    let private corelib : Lazy<DumpedAssembly> =
        lazy
            (let _, loggerFactory = LoggerFactory.makeTest ()
             Assembly.readFile loggerFactory typeof<obj>.Assembly.Location)

    let private voidType : Type = typeof<obj>.Assembly.GetType ("System.Void", true)

    /// The reflection type a stub's signature names, with `!!i` as the emitted method's own
    /// generic parameter. Custom modifiers are dropped: they change nothing about what runs.
    let rec private reflectionType (generics : Type[]) (ty : TypeDefn) : Type =
        match ty with
        | TypeDefn.Modified m -> reflectionType generics m.Unmodified
        | TypeDefn.GenericMethodParameter i -> generics.[i]
        | TypeDefn.Byref inner -> (reflectionType generics inner).MakeByRefType ()
        | TypeDefn.Pointer TypeDefn.Void -> voidType.MakePointerType ()
        | TypeDefn.Pointer inner -> (reflectionType generics inner).MakePointerType ()
        | TypeDefn.Void -> voidType
        | TypeDefn.PrimitiveType PrimitiveType.Boolean -> typeof<bool>
        | TypeDefn.PrimitiveType PrimitiveType.Byte -> typeof<byte>
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> typeof<int>
        | TypeDefn.PrimitiveType PrimitiveType.UInt32 -> typeof<uint32>
        | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> typeof<nativeint>
        | TypeDefn.PrimitiveType PrimitiveType.UIntPtr -> typeof<unativeint>
        | TypeDefn.PrimitiveType PrimitiveType.Object -> typeof<obj>
        | other -> failwith $"TestUnsafeStubs: no reflection type for %O{other} in an Unsafe signature"

    /// Emit `instruction`, one of the stub's, with its `!!0` operand as `generics.[0]`.
    let private emit (il : ILGenerator) (generics : Type[]) (instruction : IlOp) : unit =
        let genericArgument (operand : MetadataOperand) : Type =
            match operand with
            | MetadataOperand.FromMetadata token ->
                match token.Token with
                | MetadataToken.TypeSpecification spec ->
                    match corelib.Force().TypeSpecs.[spec].Signature with
                    | TypeDefn.GenericMethodParameter 0 -> generics.[0]
                    | other -> failwith $"TestUnsafeStubs: a stub's type operand is %O{other}, not !!0"
                | other -> failwith $"TestUnsafeStubs: a stub's type operand is %O{other}, not a TypeSpec"
            | MetadataOperand.FromDynamicScope _ -> failwith "TestUnsafeStubs: a stub has a dynamic-scope operand"

        match instruction with
        | IlOp.Nullary op ->
            let opcode =
                match op with
                | NullaryIlOp.LdArg0 -> OpCodes.Ldarg_0
                | NullaryIlOp.LdArg1 -> OpCodes.Ldarg_1
                | NullaryIlOp.LdArg2 -> OpCodes.Ldarg_2
                | NullaryIlOp.LdcI4_0 -> OpCodes.Ldc_I4_0
                | NullaryIlOp.Conv_I -> OpCodes.Conv_I
                | NullaryIlOp.Conv_U -> OpCodes.Conv_U
                | NullaryIlOp.Add -> OpCodes.Add
                | NullaryIlOp.Sub -> OpCodes.Sub
                | NullaryIlOp.Mul -> OpCodes.Mul
                | NullaryIlOp.Ceq -> OpCodes.Ceq
                | NullaryIlOp.Cgt_un -> OpCodes.Cgt_Un
                | NullaryIlOp.Clt_un -> OpCodes.Clt_Un
                | NullaryIlOp.Cpblk -> OpCodes.Cpblk
                | NullaryIlOp.Initblk -> OpCodes.Initblk
                | NullaryIlOp.Ret -> OpCodes.Ret
                | other -> failwith $"TestUnsafeStubs: no emitter for %O{other}"

            il.Emit opcode
        | IlOp.UnaryConst (UnaryConstIlOp.Unaligned alignment) -> il.Emit (OpCodes.Unaligned, alignment)
        | IlOp.UnaryMetadataToken (op, operand) ->
            let opcode =
                match op with
                | UnaryMetadataTokenIlOp.Sizeof -> OpCodes.Sizeof
                | UnaryMetadataTokenIlOp.Ldobj -> OpCodes.Ldobj
                | UnaryMetadataTokenIlOp.Stobj -> OpCodes.Stobj
                | UnaryMetadataTokenIlOp.Unbox -> OpCodes.Unbox
                | other -> failwith $"TestUnsafeStubs: no emitter for %O{other}"

            il.Emit (opcode, genericArgument operand)
        | other -> failwith $"TestUnsafeStubs: no emitter for %O{other}"

    /// A `Stubs` class holding, for every `Unsafe` method with a VM stub, a method of the same name
    /// and signature whose body is that stub.
    let private fabricate () : byte[] =
        let corelib = corelib.Force ()

        let builder =
            PersistedAssemblyBuilder (AssemblyName fabricatedName, typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule fabricatedName

        let stubs =
            modul.DefineType ("Stubs", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let unsafeType =
            corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "Unsafe"
            |> Option.defaultWith (fun () -> failwith "CoreLib has no Unsafe")

        let mutable emitted = 0

        for definition in unsafeType.Methods do
            match definition.TryMetadata with
            | None -> ()
            | Some facts ->

            match VmSubstitution.unsafeStub corelib facts.Handle with
            | None -> ()
            | Some stub ->
                let method =
                    stubs.DefineMethod (
                        definition.Name,
                        MethodAttributes.Public
                        ||| MethodAttributes.Static
                        ||| MethodAttributes.HideBySig
                    )

                let generics : Type[] =
                    match definition.Signature.GenericParameterCount with
                    | 0 -> [||]
                    | n ->
                        method.DefineGenericParameters (Array.init n (sprintf "T%d"))
                        |> Array.map (fun p -> p :> Type)

                method.SetReturnType (
                    match definition.Signature.ReturnType with
                    | MethodReturnType.Void -> voidType
                    | MethodReturnType.Returns ty -> reflectionType generics ty
                )

                method.SetParameters (
                    definition.Signature.ParameterTypes
                    |> List.map (reflectionType generics)
                    |> Array.ofList
                )

                let il = method.GetILGenerator ()

                for instruction, _ in stub.Instructions do
                    emit il generics instruction

                emitted <- emitted + 1

        if emitted <> 40 then
            failwith $"TestUnsafeStubs: emitted %d{emitted} stubs, where corelib.h binds 40"

        stubs.CreateType () |> ignore<Type>
        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private fabricated : Lazy<byte[]> = lazy (fabricate ())

    /// Each case is a method body returning 0 when every `Unsafe` call it makes agrees with the
    /// same call to `Stubs`, and a distinct non-zero code at the first that does not.
    let cases : TestCaseData list =
        [
            "Add and Subtract, by element",
            """
int[] a = { 10, 20, 30, 40, 50 };
if (Unsafe.Add(ref a[1], 2) != Stubs.Add(ref a[1], 2)) return 1;
if (Unsafe.Add(ref a[1], (nint)2) != Stubs.Add(ref a[1], (nint)2)) return 2;
if (Unsafe.Add(ref a[1], (nuint)2) != Stubs.Add(ref a[1], (nuint)2)) return 3;
if (Unsafe.Subtract(ref a[3], 2) != Stubs.Subtract(ref a[3], 2)) return 4;
if (Unsafe.Subtract(ref a[3], (nint)2) != Stubs.Subtract(ref a[3], (nint)2)) return 5;
if (Unsafe.Subtract(ref a[3], (nuint)2) != Stubs.Subtract(ref a[3], (nuint)2)) return 6;
Stubs.Add(ref a[0], 4) = 9;
if (a[4] != 9) return 7;
Stubs.Subtract(ref a[4], 4) = 8;
if (a[0] != 8) return 8;
return 0;
"""

            "AddByteOffset, SubtractByteOffset and ByteOffset",
            """
int[] a = { 10, 20, 30, 40, 50 };
if (Unsafe.AddByteOffset(ref a[0], (nint)8) != Stubs.AddByteOffset(ref a[0], (nint)8)) return 1;
if (Unsafe.AddByteOffset(ref a[0], (nuint)8) != Stubs.AddByteOffset(ref a[0], (nuint)8)) return 2;
if (Unsafe.SubtractByteOffset(ref a[3], (nint)8) != Stubs.SubtractByteOffset(ref a[3], (nint)8)) return 3;
if (Unsafe.SubtractByteOffset(ref a[3], (nuint)8) != Stubs.SubtractByteOffset(ref a[3], (nuint)8)) return 4;
if (Unsafe.ByteOffset(ref a[0], ref a[3]) != Stubs.ByteOffset(ref a[0], ref a[3])) return 5;
if (Unsafe.ByteOffset(ref a[3], ref a[0]) != Stubs.ByteOffset(ref a[3], ref a[0])) return 6;
return 0;
"""

            "AreSame, IsAddressGreaterThan and IsAddressLessThan",
            """
int[] a = { 10, 20, 30 };
if (Unsafe.AreSame(ref a[1], ref a[1]) != Stubs.AreSame(ref a[1], ref a[1])) return 1;
if (Unsafe.AreSame(ref a[1], ref a[2]) != Stubs.AreSame(ref a[1], ref a[2])) return 2;
if (Unsafe.IsAddressGreaterThan(ref a[2], ref a[1]) != Stubs.IsAddressGreaterThan(ref a[2], ref a[1])) return 3;
if (Unsafe.IsAddressGreaterThan(ref a[1], ref a[2]) != Stubs.IsAddressGreaterThan(ref a[1], ref a[2])) return 4;
if (Unsafe.IsAddressLessThan(ref a[2], ref a[1]) != Stubs.IsAddressLessThan(ref a[2], ref a[1])) return 5;
if (Unsafe.IsAddressLessThan(ref a[1], ref a[2]) != Stubs.IsAddressLessThan(ref a[1], ref a[2])) return 6;
return 0;
"""

            "NullRef and IsNullRef",
            """
int[] a = { 10 };
if (!Stubs.IsNullRef(ref Stubs.NullRef<int>())) return 1;
if (!Unsafe.IsNullRef(ref Stubs.NullRef<int>())) return 2;
if (!Stubs.IsNullRef(ref Unsafe.NullRef<int>())) return 3;
if (Stubs.IsNullRef(ref a[0])) return 4;
return 0;
"""

            "As, between byrefs",
            """
long l = 0x0000000200000001L;
if (Unsafe.As<long, int>(ref l) != Stubs.As<long, int>(ref l)) return 1;
Stubs.As<long, int>(ref l) = 7;
if (l != 0x0000000200000007L) return 2;
return 0;
"""

            "As, on an object reference",
            """
object o = "hello";
if (!ReferenceEquals(Unsafe.As<string>(o), Stubs.As<string>(o))) return 1;
return 0;
"""

            "AsRef and SkipInit",
            """
int[] a = { 10, 20, 30 };
Stubs.AsRef(ref a[2]) = 33;
if (a[2] != 33) return 1;
int x = 5;
Stubs.SkipInit(ref x);
if (x != 5) return 2;
return 0;
"""

            "Read, Write, ReadUnaligned and WriteUnaligned through pointers",
            """
int[] a = { 10, 20, 30, 40 };
fixed (int* p = a)
{
    if (Unsafe.Read<int>(p + 1) != Stubs.Read<int>(p + 1)) return 1;
    Stubs.Write<int>(p + 2, 99);
    if (a[2] != 99) return 2;
    if (Unsafe.ReadUnaligned<int>(p + 3) != Stubs.ReadUnaligned<int>(p + 3)) return 3;
    Stubs.WriteUnaligned<int>(p, 77);
    if (a[0] != 77) return 4;
}
return 0;
"""

            "ReadUnaligned through a byte byref",
            """
byte[] b = { 0, 1, 2, 3, 4, 5 };
if (Unsafe.ReadUnaligned<int>(ref b[1]) != Stubs.ReadUnaligned<int>(ref b[1])) return 1;
return 0;
"""

            "Add, Subtract and AsPointer on pointers",
            """
int[] a = { 10, 20, 30, 40 };
fixed (int* p = a)
{
    if (Unsafe.Add<int>(p, 2) != Stubs.Add<int>(p, 2)) return 1;
    if (Unsafe.Subtract<int>(p + 3, 2) != Stubs.Subtract<int>(p + 3, 2)) return 2;
    if (Stubs.AsPointer(ref a[1]) != (void*)(p + 1)) return 3;
}
return 0;
"""

            "Copy between a pointer and a byref",
            """
int[] a = { 10, 20, 30, 40 };
fixed (int* p = a)
{
    int v = 5;
    Stubs.Copy(p + 3, ref v);
    if (a[3] != 5) return 1;
    Stubs.Copy(ref v, p);
    if (v != 10) return 2;
}
return 0;
"""

            "CopyBlock and CopyBlockUnaligned",
            """
byte[] b = { 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0 };
Stubs.CopyBlock(ref b[4], ref b[0], 4);
if (b[4] != 1 || b[7] != 4) return 1;
Stubs.CopyBlockUnaligned(ref b[9], ref b[1], 3);
if (b[9] != 2 || b[11] != 4) return 2;
fixed (byte* p = b)
{
    Stubs.CopyBlock(p + 8, p, 2);
    if (b[8] != 1 || b[9] != 2) return 3;
    Stubs.CopyBlockUnaligned(p + 1, p + 5, 2);
    if (b[1] != 2 || b[2] != 3) return 4;
}
return 0;
"""

            "InitBlock and InitBlockUnaligned",
            """
byte[] b = new byte[8];
Stubs.InitBlock(ref b[1], 7, 3);
if (b[0] != 0 || b[1] != 7 || b[3] != 7 || b[4] != 0) return 1;
Stubs.InitBlockUnaligned(ref b[4], 9, 2);
if (b[4] != 9 || b[5] != 9 || b[6] != 0) return 2;
fixed (byte* p = b)
{
    Stubs.InitBlock(p, 1, 2);
    if (b[0] != 1 || b[1] != 1 || b[2] != 7) return 3;
    Stubs.InitBlockUnaligned(p + 6, 5, 2);
    if (b[6] != 5 || b[7] != 5) return 4;
}
return 0;
"""

            "Unbox",
            """
object boxed = 42;
if (Unsafe.Unbox<int>(boxed) != Stubs.Unbox<int>(boxed)) return 1;
Stubs.Unbox<int>(boxed) = 43;
if ((int)boxed != 43) return 2;
return 0;
"""
        ]
        |> List.map (fun (name, body) -> TestCaseData(name, body).SetArgDisplayNames name)

    /// The `Unsafe` methods whose stub PawPrint cannot run, so that `Intrinsics.call`'s own
    /// implementation is the only way PawPrint runs them: each is a case the real runtime passes
    /// and PawPrint refuses, with the refusal it gives. When PawPrint comes to run one of these
    /// stubs, its case here fails, and belongs in `cases` instead.
    let overrides : TestCaseData list =
        [
            // `stobj int32` through a `ref byte`: PawPrint's store refuses to write more bytes than
            // the slot the byref names holds, where `Intrinsics.call`'s `WriteUnaligned` spreads the
            // value over the array's consecutive elements.
            "WriteUnaligned through a byte byref",
            """
byte[] b = new byte[12];
Unsafe.WriteUnaligned(ref b[1], 0x04030201);
if (b[1] != 1 || b[4] != 4) return 1;
Stubs.WriteUnaligned(ref b[5], 0x04030201);
if (b[5] != 1 || b[8] != 4) return 2;
return 0;
""",
            "would overrun it"
        ]
        |> List.map (fun (name, body, refusal) -> TestCaseData(name, body, refusal).SetArgDisplayNames name)

    let private driverOf (body : string) : string =
        $"""
using System;
using System.Runtime.CompilerServices;

public static unsafe class Driver
{{
    public static int Main()
    {{
{body}
    }}
}}
"""

    [<TestCaseSource(nameof cases)>]
    let ``an Unsafe method agrees with the VM stub Semantics transcribes`` (name : string) (body : string) : unit =
        FabricatedGuest.run fabricatedName (fabricated.Force ()) "UnsafeStubsDriver" (driverOf body) 0

    [<TestCaseSource(nameof overrides)>]
    let ``PawPrint cannot yet run the VM stub of an Unsafe method it implements itself``
        (name : string)
        (body : string)
        (refusal : string)
        : unit
        =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth fabricatedName (fabricated.Force ()) "UnsafeStubsDriver" (driverOf body)

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)

        match onPawPrint with
        | FabricatedOutcome.Failed e -> e.ToString () |> shouldContainText refusal
        | FabricatedOutcome.Exited code ->
            failwith
                $"PawPrint ran %s{name} to exit code %d{code}: if that is 0, it runs this stub, and the case belongs in `cases`"
