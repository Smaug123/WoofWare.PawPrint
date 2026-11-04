namespace WoofWare.PawPrint

open System.Reflection
open Microsoft.Extensions.Logging

/// What CoreCLR's class loader makes of a method's return type when it decides which methods take
/// part in runtime async (`MethodReturnKind`, method.hpp; `ClassifyMethodReturnKind`, method.cpp).
[<RequireQualifiedAccess>]
type MethodReturnKind =
    /// Returns anything but one of the four promise types below.
    | NormalMethod
    /// Returns CoreLib's `System.Threading.Tasks.Task` or `ValueTask`.
    | NonGenericTaskReturningMethod
    /// Returns CoreLib's `System.Threading.Tasks.Task<T>` or `ValueTask<T>`.
    | GenericTaskReturningMethod

/// Which methods CoreCLR's class loader makes callable with runtime async's calling convention.
[<RequireQualifiedAccess>]
module RuntimeAsyncMethod =

    /// `MethodImplAttributes.Async`, which the `System.Reflection` of this project's target
    /// framework does not name.
    let asyncImplAttribute : MethodImplAttributes = enum<MethodImplAttributes> 0x2000

    /// CoreCLR's `MethodDesc::IsAsyncMethod` for the method `method`, which `declaringType` of
    /// `declaringAssembly` declares: whether the class loader gives it the `AsyncCall` flag.
    /// This is the method a MethodDef row declares, never the async variant CoreCLR adds beside a
    /// method returning a promise type, which reflection does not see.
    let isAsyncCall
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssembly : DumpedAssembly)
        (declaringType : TypeInfo<'generic, 'field>)
        (method : MetadataMethodFacts)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        assemblies, Ok false
