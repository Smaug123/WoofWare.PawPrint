namespace WoofWare.PawPrint.Analysis

open WoofWare.PawPrint

/// What the marshalling walk (`MarshalingRequired`) reads of a value type, from what the type
/// system computes of it.
[<RequireQualifiedAccess>]
module internal MarshalingFacts =

    /// The facts of `concrete`, a value type whose row is `typeInfo` and whose layout facts are
    /// `facts`, where `coreLib` is the CoreLib the analysis runs over.
    let ofLayoutFacts
        (coreLib : DumpedAssembly)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (facts : ValueTypeLayoutFacts)
        : ValueTypeFacts
        =
        let instantiation =
            if concrete.Generics.IsEmpty then
                ValueTypeInstantiation.NotGeneric
            elif concrete.AssemblyFullName = coreLib.DefinitionFullName && not typeInfo.IsNested then
                ValueTypeInstantiation.Generic (Some (concrete.Namespace, concrete.Name))
            else
                ValueTypeInstantiation.Generic None

        {
            IsEnum = facts.IsEnum
            IsBlittable = facts.IsBlittable
            ContainsGcPointers = facts.ContainsGcPointers
            IsAutoLayoutOrHasAutoLayoutField = facts.IsAutoLayoutOrHasAutoLayoutField
            IsInt128OrHasInt128Fields = facts.IsInt128OrHasInt128Fields
            IsDecimalFloatingPointOrHasDecimalFloatingPointFields =
                facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields
            Instantiation = instantiation
            SizeUpperBound = facts.SizeUpperBound
        }
