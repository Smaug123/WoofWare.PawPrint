namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A metadata-keyed field lookup can miss for two very different reasons, and the caller cannot
/// tell them apart from "not found": either the object genuinely has no such field, or it has
/// exactly that field definition but keyed to a *different* declaring-type instantiation. The
/// second is the interesting one -- it means the access site and the storage layout disagreed
/// about the declaring type's generic arguments -- so `CliValueType.FindFieldById` names it
/// explicitly. These tests pin that, because a diagnostic nobody exercises rots silently.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFieldIdDiagnostics =

    let private corelib : DumpedAssembly =
        // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
        // its sinks, and disposing while the assembly is still live would silently drop events.
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) bct AllConcreteTypes.Empty

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ti

    let private intPtrValueField () : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        bct.IntPtr.Fields
        |> List.filter (fun field -> field.Name = "_value" && not field.IsStatic)
        |> List.exactlyOne

    /// A one-field `System.IntPtr` value, whose single field is keyed to `declaringType`.
    /// Passing a `declaringType` other than IntPtr's own handle is exactly the corruption these
    /// tests need to simulate: storage laid out against one instantiation, read against another.
    let private intPtrValueKeyedTo (declaringType : ConcreteTypeHandle) : CliValueType =
        let valueField = intPtrValueField ()

        [
            {
                Id = FieldId.metadata declaringType valueField.Handle valueField.Name
                Name = valueField.Name
                Contents = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
                Offset = valueField.Offset
                Type = handleFor bct.IntPtr
                MarshallingDescriptor = None
            }
        ]
        |> CliValueType.OfFields bct concreteTypes (handleFor bct.IntPtr) Layout.Default CharSet.Ansi

    [<Test>]
    let ``a declaring-type mismatch is reported as such, not as a bare "not found"`` () : unit =
        let valueField = intPtrValueField ()

        // Storage was laid out keying the field to IntPtr; the access site asks for the same
        // field definition keyed to Int32. This is the shape of every "the access site
        // concretized the declaring type differently" bug.
        let stored = intPtrValueKeyedTo (handleFor bct.IntPtr)

        let requested =
            FieldId.metadata (handleFor bct.Int32) valueField.Handle valueField.Name

        let exn =
            Assert.Throws (fun () -> CliValueType.DereferenceFieldById requested stored |> ignore)

        let message = exn.Message

        message |> shouldContainText "not found"
        // The smoking gun: same field definition, different declaring type.
        message |> shouldContainText "The same field definition IS present in storage"
        // And the actual identity present, so the reader can diff the two handles by eye.
        message
        |> shouldContainText (string (FieldId.metadata (handleFor bct.IntPtr) valueField.Handle valueField.Name))

    [<Test>]
    let ``a genuinely absent field does not claim a declaring-type mismatch`` () : unit =
        // `System.Int32`'s instance field is a different field definition entirely, so there is
        // no same-definition entry to point at and the message must not invent one.
        let int32Field =
            bct.Int32.Fields
            |> List.filter (fun field -> not field.IsStatic)
            |> List.exactlyOne

        let stored = intPtrValueKeyedTo (handleFor bct.IntPtr)

        let requested =
            FieldId.metadata (handleFor bct.Int32) int32Field.Handle int32Field.Name

        let exn =
            Assert.Throws (fun () -> CliValueType.DereferenceFieldById requested stored |> ignore)

        exn.Message |> shouldContainText "not found"

        exn.Message.Contains "The same field definition IS present in storage"
        |> shouldEqual false

    [<Test>]
    let ``the failure message lists the identities that are present`` () : unit =
        let valueField = intPtrValueField ()
        let stored = intPtrValueKeyedTo (handleFor bct.IntPtr)

        let requested =
            FieldId.metadata (handleFor bct.Int32) valueField.Handle valueField.Name

        let exn =
            Assert.Throws (fun () -> CliValueType.DereferenceFieldById requested stored |> ignore)

        exn.Message |> shouldContainText "Available field identities:"
        // The declared type of the value being read is part of the picture too: without it you
        // cannot tell whether the *object* or the *access* was the thing keyed unexpectedly.
        exn.Message |> shouldContainText (string (handleFor bct.IntPtr))
