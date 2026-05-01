namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

type internal UnaryMetadataIlOpContext =
    {
        LoggerFactory : ILoggerFactory
        BaseClassTypes : BaseClassTypes<DumpedAssembly>
        Op : UnaryMetadataTokenIlOp
        ActiveAssembly : DumpedAssembly
        MetadataToken : MetadataToken
        CurrentMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        Thread : ThreadId
        Logger : ILogger
    }
