namespace WoofWare.PawPrint

/// Which arguments CoreCLR admits in no generic instantiation (`Generics::CheckInstantiation`,
/// generics.cpp), where that differs between runtimes. The check reads each argument's element
/// type, and refuses `ELEMENT_TYPE_BYREF`, `TYPEDBYREF`, `VOID`, `PTR` and `FNPTR` on every runtime.
[<RequireQualifiedAccess>]
module InstantiationCheck =

    /// Whether `runtime`'s CoreCLR refuses `System.TypedReference` as an argument. .NET 10's
    /// reports its element type as `ELEMENT_TYPE_TYPEDBYREF`; .NET 11's treats it as an ordinary
    /// value type, which a parameter allowing a byref-like argument admits.
    let refusesTypedReference (runtime : EmulatedRuntime) : bool =
        match runtime with
        | EmulatedRuntime.Net10 -> true
        | EmulatedRuntime.Net11 -> false
