namespace WoofWare.PawPrint

open System.IO

/// The fields of CoreLib that CoreCLR's native code binds by name, through the rows of its
/// `vm/corelib.h`: each `DEFINE_FIELD`, by which the VM finds a field to read or write, and each
/// `DEFINE_FIELD_U`, which checks that a managed field lies where a field of a native object does.
/// The VM may write any of them where no instruction names it, as `CastCache::Initialize` stores
/// the cast cache's table in `CastHelpers.s_table`. It is a lower bound on what the VM writes, not
/// the whole of it: the VM reaches a field through the C++ layout of its class (`vm/object.h`)
/// whether or not a row checks it, as `ReflectFieldObject::SetKeepAlive` writes
/// `RuntimeFieldInfoStub.m_keepalive`, for which `corelib.h` has no row. The rows of every supported
/// runtime at once, from the embedded `VmBoundFields.tsv`, which `TestVmBoundField` holds to each
/// runtime's `corelib.h`.
[<RequireQualifiedAccess>]
module VmBoundField =

    /// Each such field, by its declaring type's full name (`TypeInfo.fullName`) and its own name.
    let all : Lazy<Set<string * string>> =
        lazy
            (let assembly = typeof<EmulatedRuntime>.Assembly
             let name = "WoofWare.PawPrint.Semantics.VmBoundFields.tsv"

             use stream =
                 match assembly.GetManifestResourceStream name with
                 | null -> failwith $"VmBoundField: %s{assembly.FullName} embeds no %s{name}"
                 | stream -> stream

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split '\n'
             |> Seq.map (fun line -> line.TrimEnd '\r')
             |> Seq.filter (fun line -> line <> "" && not (line.StartsWith '#'))
             |> Seq.map (fun line ->
                 match line.Split '\t' with
                 | [| typeName ; fieldName |] -> typeName, fieldName
                 | _ -> failwith $"VmBoundField: a row of %s{name} is not two tab-separated fields: %s{line}"
             )
             |> Set.ofSeq)

    /// Whether CoreCLR's native code binds the field of this name of the CoreLib type of this full
    /// name (`TypeInfo.fullName`).
    let isBound (typeFullName : string) (fieldName : string) : bool =
        (all.Force ()).Contains (typeFullName, fieldName)
