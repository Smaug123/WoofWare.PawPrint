namespace WoofWare.PawPrint

open System.IO

/// The CoreLib types whose objects CoreCLR's native code lays out as a C++ class of its own
/// (`vm/object.h` and its neighbours: `StringObject` for `System.String`, `ExceptionObject` for
/// `System.Exception`, `ReflectFieldObject` for `System.RuntimeFieldInfoStub`), through which it
/// reads and writes any instance field of the type where no instruction names the field, as
/// `ReflectFieldObject::SetKeepAlive` writes `RuntimeFieldInfoStub.m_keepalive`. Most such classes
/// `vm/corelib.h` checks against the managed layout with a `DEFINE_CLASS_U` row, and only the fields
/// its `DEFINE_FIELD_U` rows name are in `VmBoundField`; the rest it does not check, and no row
/// names. The types of every supported runtime at once, from the embedded `VmLaidOutClasses.tsv`,
/// which `TestVmLaidOutClass` holds to each runtime's `corelib.h` and C++ sources.
[<RequireQualifiedAccess>]
module VmLaidOutClass =

    /// Each such type, by its full name (`TypeInfo.fullName`), with the C++ class that lays it out.
    let all : Lazy<Set<string * string>> =
        lazy
            (let assembly = typeof<EmulatedRuntime>.Assembly
             let name = "WoofWare.PawPrint.Semantics.VmLaidOutClasses.tsv"

             use stream =
                 match assembly.GetManifestResourceStream name with
                 | null -> failwith $"VmLaidOutClass: %s{assembly.FullName} embeds no %s{name}"
                 | stream -> stream

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split '\n'
             |> Seq.map (fun line -> line.TrimEnd '\r')
             |> Seq.filter (fun line -> line <> "" && not (line.StartsWith '#'))
             |> Seq.map (fun line ->
                 match line.Split '\t' with
                 | [| typeName ; nativeClass |] -> typeName, nativeClass
                 | _ -> failwith $"VmLaidOutClass: a row of %s{name} is not two tab-separated fields: %s{line}"
             )
             |> Set.ofSeq)

    let private types : Lazy<Set<string>> = lazy (all.Force () |> Set.map fst)

    /// Whether CoreCLR's native code lays out the CoreLib type of this full name
    /// (`TypeInfo.fullName`) as a C++ class, so may write any instance field of it.
    let isLaidOut (typeFullName : string) : bool = (types.Force ()).Contains typeFullName
