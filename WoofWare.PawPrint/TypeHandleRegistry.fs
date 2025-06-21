namespace WoofWare.PawPrint

[<Measure>]
type typeHandle

type CanonicalTypeIdentity =
    {
        AssemblyFullName : string
        FullyQualifiedTypeName : string
        Generics : CanonicalTypeIdentity list
    }

type TypeHandleRegistry =
    {
        TypeHandleToType : Map<int64<typeHandle>, CanonicalTypeIdentity>
        TypeToHandle : Map<CanonicalTypeIdentity, int64<typeHandle>>
        NextHandle : int64<typeHandle>
    }

[<RequireQualifiedAccess>]
module TypeHandleRegistry =
    let empty () =
        {
            TypeHandleToType = Map.empty
            TypeToHandle = Map.empty
            NextHandle = 1L<typeHandle>
        }

    let getOrAllocate
        (def : CanonicalTypeIdentity)
        (reg : TypeHandleRegistry)
        : int64<typeHandle> * TypeHandleRegistry
        =
        match Map.tryFind def reg.TypeToHandle with
        | Some v -> v, reg
        | None ->

        let handle = reg.NextHandle

        let reg =
            {
                NextHandle = handle + 1L<typeHandle>
                TypeHandleToType = reg.TypeHandleToType |> Map.add handle def
                TypeToHandle = reg.TypeToHandle |> Map.add def handle
            }

        handle, reg
