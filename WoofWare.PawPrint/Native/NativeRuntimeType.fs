namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeType =
    let getOrAllocateNonGenericRuntimeType =
        NativeRuntimeTypeHelpers.getOrAllocateNonGenericRuntimeType

    let getOrAllocateRuntimeModule = NativeRuntimeTypeHelpers.getOrAllocateRuntimeModule
    let tryExecuteQCall = NativeRuntimeTypeQCall.tryExecute
    let tryExecute = NativeRuntimeTypeFCall.tryExecute
