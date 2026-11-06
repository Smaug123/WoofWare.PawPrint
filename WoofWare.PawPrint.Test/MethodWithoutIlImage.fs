namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped

/// An assembly holding methods whose rows are managed IL with no body: not abstract, and an RVA of
/// 0, as a dynamic module's method is from `TypeBuilder_DefineMethod` until
/// `TypeBuilder_SetMethodIL`. C# cannot spell one and `PersistedAssemblyBuilder` will not save one,
/// so each such method is saved with a body and its RVA zeroed afterwards.
[<RequireQualifiedAccess>]
module MethodWithoutIlImage =

    let name : string = "MethodWithoutIl"

    /// Zero the RVA of every method of these names that has a body, and add `Virtual` to the flags
    /// of each named in `madeVirtual`, which `PersistedAssemblyBuilder` refuses for a static method.
    let private withoutBodies (names : string list) (madeVirtual : string list) (image : byte[]) : byte[] =
        let patched = Array.copy image
        use pe = new PEReader (ImmutableArray.Create<byte> image)
        let reader = pe.GetMetadataReader ()

        let tableStart =
            pe.PEHeaders.MetadataStartOffset
            + reader.GetTableMetadataOffset TableIndex.MethodDef

        let rowSize = reader.GetTableRowSize TableIndex.MethodDef

        let zeroed =
            [
                for handle in reader.MethodDefinitions do
                    let definition = reader.GetMethodDefinition handle
                    let name = reader.GetString definition.Name

                    if List.contains name names && definition.RelativeVirtualAddress <> 0 then
                        let offset =
                            tableStart
                            + (MetadataTokens.GetRowNumber (MethodDefinitionHandle.op_Implicit handle : EntityHandle)
                               - 1)
                              * rowSize

                        // The row's columns are the RVA (four bytes), ImplFlags (two) and Flags (two).
                        Array.fill patched offset 4 0uy

                        if List.contains name madeVirtual then
                            let flags = uint16 (definition.Attributes ||| MethodAttributes.Virtual)

                            patched.[offset + 6] <- byte flags
                            patched.[offset + 7] <- byte (flags >>> 8)

                        yield name
            ]

        List.sort zeroed |> shouldEqual (List.sort names)
        patched

    /// The image: `Holder.NoBody()`, a static method; `IThing.DefaultRun()`, an interface's default
    /// body; and `IThing.StaticRun()`, a static virtual default body, each with no IL; `Thing`, a
    /// class implementing `IThing` with neither; and `Reacher.ReachUnrelated()`, which calls
    /// `IThing.StaticRun()` constrained to `object`.
    let fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule name

        let ret (m : MethodBuilder) : unit = m.GetILGenerator().Emit OpCodes.Ret

        let holder =
            modul.DefineType ("Holder", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let noBody =
            holder.DefineMethod (
                "NoBody",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                typeof<Void>,
                Type.EmptyTypes
            )

        ret noBody
        holder.CreateType () |> ignore<Type>

        // An interface whose default instance and static bodies have no IL.
        let iface =
            modul.DefineType ("IThing", TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        let defaultRun =
            iface.DefineMethod (
                "DefaultRun",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.HideBySig,
                typeof<Void>,
                Type.EmptyTypes
            )

        ret defaultRun

        // Made virtual once saved: a static virtual method with a body is a default implementation.
        let staticRun =
            iface.DefineMethod (
                "StaticRun",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                typeof<Void>,
                Type.EmptyTypes
            )

        ret staticRun
        iface.CreateType () |> ignore<Type>

        // `constrained. object call IThing::StaticRun()`: a constrained type that does not implement
        // the interface, which C# cannot spell.
        let reacher =
            modul.DefineType ("Reacher", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let reachUnrelated =
            reacher.DefineMethod (
                "ReachUnrelated",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                typeof<Void>,
                Type.EmptyTypes
            )

        let il = reachUnrelated.GetILGenerator ()
        il.Emit (OpCodes.Constrained, typeof<obj>)
        il.Emit (OpCodes.Call, staticRun)
        il.Emit OpCodes.Ret
        reacher.CreateType () |> ignore<Type>

        let thing =
            modul.DefineType ("Thing", TypeAttributes.Public, typeof<obj>, [| iface :> Type |])

        thing.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        thing.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        withoutBodies [ "NoBody" ; "DefaultRun" ; "StaticRun" ] [ "StaticRun" ] (stream.ToArray ())
