# Type Concretization System

Guidance for working with the type concretization subsystem: converting abstract type definitions (`TypeDefn`) to concrete runtime types (`ConcreteTypeHandle`), resolving generics, and debugging type resolution issues.

## Overview

Type concretization converts abstract type definitions (`TypeDefn`) to concrete runtime types (`ConcreteTypeHandle`). This is essential because IL operations need exact types at runtime, including all generic instantiations. The system separates type concretization from IL execution, ensuring types are properly loaded before use.

## Key Concepts

### Generic Parameters
- **Common error**: "Generic type/method parameter X out of range" probably means you're missing the proper generic context: some caller has passed the wrong list of generics through somewhere.

### Assembly Context
TypeRefs must be resolved in the context of the assembly where they're defined, not where they're used. When resolving a TypeRef, always use the assembly that contains the TypeRef in its metadata.

## Common Scenarios and Solutions

### Nested Generic Contexts
When inside `Array.Empty<T>()` calling `AsRef<T>`, the `T` refers to the outer method's generic parameter. Pass the current executing method's generics as context:
```fsharp
let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod
concretizeMethodWithTypeGenerics ... currentMethod.Generics state
```

### Field Access in Generic Contexts
When accessing `EmptyArray<T>.Value` from within `Array.Empty<T>()`, use both type and method generics:
```fsharp
let contextTypeGenerics = currentMethod.DeclaringType.Generics
let contextMethodGenerics = currentMethod.Generics
```

### callMethod
- `callMethod` expects already-concretized methods (of type `MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>`)
- Callers must concretize (via `concretizeMethodForExecution`) and ensure type initialization before calling

## Common Pitfalls

1. **Don't create new generic parameters when they already exist**. It's *very rarely* correct to instantiate `TypeDefn.Generic{Type,Method}Parameter` yourself:
   ```fsharp
   // Wrong: field.DeclaringType.Generics |> List.mapi (fun i _ -> TypeDefn.GenericTypeParameter i)
   // Right: field.DeclaringType.Generics
   ```

2. **Assembly loading context**: The `loadAssembly` function expects the assembly that contains the reference as the first parameter, not the target assembly

3. **Type forwarding**: Use `Assembly.resolveTypeRef` which handles type forwarding and exported types correctly

## Key Files

- **TypeConcretisation.fs**: Core type concretization logic
  - `concretizeType`: Main entry point
  - `concretizeGenericInstantiation`: Handles generic instantiations like `List<T>`
  - `ConcretizationContext`: Tracks state during concretization

- **IlMachineState.fs**:
  - `concretizeMethodForExecution`: Prepares methods for execution
  - `concretizeFieldForExecution`: Prepares fields for access
  - Manages the flow of generic contexts through execution

- **Assembly.fs**:
  - `resolveTypeRef`: Resolves type references across assemblies
  - `resolveTopLevelTypeFromName`: Discovers top-level types (by namespace + name), falling back to exported types
  - `resolveTypeFromExport`: Follows type forwarding chains

## Debugging Type Concretization Issues

When encountering errors:
1. Check the generic context (method name, generic parameters)
2. Verify the assembly context being used
3. Identify the TypeDefn variant being concretized
4. Add logging to see generic contexts: `failwithf "Failed to concretize: %A" typeDefn`
5. Check if you're in a generic method calling another generic method
6. Verify TypeRefs are being resolved in the correct assembly

## Common Type System Patterns

### Creating TypeDefn from Type Metadata

When you need to create a `TypeDefn` from type metadata (e.g., from a `TypeInfo`), there's a common pattern that involves:
1. Resolving the base type to determine `SignatureTypeKind`
2. Creating the base `TypeDefn.FromDefinition`
3. For generic types, creating a `GenericInstantiation` with type parameters

This pattern is implemented in `UnaryMetadataIlOp.lookupTypeDefn`. Example usage:
```fsharp
let state, typeDefn =
    UnaryMetadataIlOp.lookupTypeDefn
        baseClassTypes
        state
        activeAssembly
        typeDefHandle
```

### Field Signature Comparison in Generic Contexts

When comparing field signatures in generic contexts (e.g., when resolving member references), signatures must be concretized before comparison. This ensures generic type parameters are properly substituted:

```fsharp
// Concretize both signatures before comparing
let state, concreteFieldSig = concretizeType ... fieldSig
let state, fieldSigConcrete = concretizeType ... fi.Signature
if fieldSigConcrete = concreteFieldSig then ...
```

### Static vs Instance Fields

When constructing objects with `Newobj`:
- Only instance fields should be included in the object
- Static fields belong to the type, not instances
- Filter using: `field.Attributes.HasFlag FieldAttributes.Static`

Example:
```fsharp
let instanceFields =
    ctorType.Fields
    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
```
