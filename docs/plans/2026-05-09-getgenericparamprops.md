# Plan: MetadataImport._GetGenericParamProps InternalCall

## Why

`Type.GenericParameterAttributes` on a generic-parameter `RuntimeType` calls
the managed property `RuntimeType.GenericParameterAttributes` (in
RuntimeType.CoreCLR.cs:3533-3546):

```csharp
RuntimeModule module = GetRuntimeModule();
module.MetadataImport.GetGenericParamProps(MetadataToken, out GenericParameterAttributes attributes);
```

Every prerequisite in this chain is now implemented:

| Step                                 | InternalCall               | Status         |
|--------------------------------------|----------------------------|----------------|
| `IsGenericParameter` guard           | `RuntimeTypeHandle.IsGenericVariable`      | Merged (#423+) |
| `GetRuntimeModule()`                 | `RuntimeTypeHandle.GetModule`              | Merged         |
| `module.MetadataImport`              | `MetadataImport.GetMetadataImport`         | Merged         |
| `MetadataToken` property             | `RuntimeTypeHandle.GetToken`               | Merged (#437)  |
| `GetGenericParamProps(token, out …)`  | **`MetadataImport.GetGenericParamProps`**  | **THIS PR**    |

The C++ implementation (managedmdimport.cpp:259-264) is a one-liner:

```cpp
FCIMPL3(HRESULT, MetaDataImport::GetGenericParamProps,
        IMDInternalImport* pScope, mdToken tk, DWORD* pAttributes)
{
    return pScope->GetGenericParamProps(tk, NULL, pAttributes, NULL, NULL, NULL);
}
```

The managed declaration (MdImport.cs:469-470):

```csharp
[MethodImpl(MethodImplOptions.InternalCall)]
private static extern int GetGenericParamProps(IntPtr scope, int genericParameter, out int flags);
```

This is identical in shape to the existing `GetFieldDefProps` arm
(NativeMetadataImport.fs:559-588): `(IntPtr scope, int token, out int flags)
→ int`. The implementation is a near-verbatim copy, swapping
`FieldDefinition` for `GenericParameter`.

## What I learned

- `GetFieldDefProps` (NativeMetadataImport.fs:559-588) is the exact template.
  Same signature shape: `IntPtr scope`, `Int32 token`, `byref Int32 out`,
  returns `Int32` (HRESULT). Body: extract assembly from scope, decode token,
  look up metadata, write attributes to out pointer, push HRESULT 0.
- The `metadataReaderOf` helper (line 17) already returns a
  `System.Reflection.Metadata.MetadataReader` from a `DumpedAssembly`.
  `MetadataReader.GetGenericParameter(GenericParameterHandle)` returns SRM's
  `GenericParameter` struct; its `.Attributes` property is
  `GenericParameterAttributes` (the same enum the managed caller expects).
- We don't need to go through PawPrint's domain model
  (`GenericParamMetadata`) at all. The InternalCall reads raw metadata, so
  going straight through the `MetadataReader` is both correct and matches how
  `GetSigOfMethodDef` and `GetFieldDefProps` work.
- The `GenericParameterAttributes` enum (ECMA-335 §II.23.1.7) is a flags
  enum: bits for variance (1=covariant, 2=contravariant), constraints
  (4=NotNullableValueType, 8=ReferenceType, 16=DefaultConstructor,
  32=AllowByRefLike). `int param.Attributes` captures all of these.
- The IOE path (`typeof(int).GenericParameterAttributes` →
  `InvalidOperationException`) is currently blocked by the unimplemented
  QCall `RuntimeMethodHandle::IsCAVisibleFromDecoratedType` during
  ResourceManager init. The test should exercise only positive cases
  (actual generic parameters) and note the IOE gap.

## Implementation steps

1. **`WoofWare.PawPrint/Native/NativeMetadataImport.fs`** — add a new arm
   between `GetParentToken` (line 840) and `| _ -> None` (line 841).
   Following the `GetFieldDefProps` pattern:

   ```fsharp
   | "System.Private.CoreLib",
     "System.Reflection",
     "MetadataImport",
     "GetGenericParamProps",
     [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
       ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
       ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
     MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
       let operation = "MetadataImport.GetGenericParamProps"
       let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
       let assembly = metadataImportAssembly operation state assemblyFullName

       let mdToken =
           match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
           | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
           | other -> failwith $"%s{operation}: expected Int32 genericParameter argument, got %O{other}"

       let attributesOut =
           NativeCall.managedPointerOfPointerArgument
               operation
               "flags out pointer"
               instruction.Arguments.[2]

       let genericParamHandle =
           match MetadataToken.ofInt mdToken with
           | MetadataToken.GenericParameter h -> h
           | token ->
               failwith $"%s{operation}: expected GenericParameter token, got %O{token} from 0x%08x{mdToken}"

       let mr = metadataReaderOf assembly
       let genericParam = mr.GetGenericParameter genericParamHandle
       let flags = int genericParam.Attributes

       let state = writeInt32AtPointer ctx.BaseClassTypes state attributesOut flags

       let state =
           IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

       (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
   ```

2. **Test** — add `WoofWare.PawPrint.Test/sourcesPure/TypeGenericParameterAttributes.cs`.
   This exercises `Type.GenericParameterAttributes` end-to-end through the
   full InternalCall chain. The pure-test harness cross-checks against the
   real runtime.

   ```csharp
   using System;
   using System.Reflection;

   namespace TypeGenericParameterAttributes
   {
       class Box<T> { }
       class StructBox<T> where T : struct { }
       class RefBox<T> where T : class { }
       class NewBox<T> where T : new() { }

       interface ICov<out T> { }
       interface IContra<in T> { }

       class Program
       {
           static int Main(string[] args)
           {
               // Unconstrained: None (0)
               GenericParameterAttributes unc =
                   typeof(Box<>).GetGenericArguments()[0].GenericParameterAttributes;
               if (unc != GenericParameterAttributes.None) return 1;

               // where T : struct → NotNullableValueTypeConstraint | DefaultConstructorConstraint
               GenericParameterAttributes st =
                   typeof(StructBox<>).GetGenericArguments()[0].GenericParameterAttributes;
               if ((st & GenericParameterAttributes.NotNullableValueTypeConstraint) == 0) return 2;
               if ((st & GenericParameterAttributes.DefaultConstructorConstraint) == 0) return 3;

               // where T : class → ReferenceTypeConstraint
               GenericParameterAttributes rf =
                   typeof(RefBox<>).GetGenericArguments()[0].GenericParameterAttributes;
               if ((rf & GenericParameterAttributes.ReferenceTypeConstraint) == 0) return 4;

               // where T : new() → DefaultConstructorConstraint only (no struct/class flag)
               GenericParameterAttributes nw =
                   typeof(NewBox<>).GetGenericArguments()[0].GenericParameterAttributes;
               if ((nw & GenericParameterAttributes.DefaultConstructorConstraint) == 0) return 5;
               if ((nw & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0) return 6;
               if ((nw & GenericParameterAttributes.ReferenceTypeConstraint) != 0) return 7;

               // out T (covariant)
               GenericParameterAttributes co =
                   typeof(ICov<>).GetGenericArguments()[0].GenericParameterAttributes;
               if ((co & GenericParameterAttributes.Covariant) == 0) return 8;
               if ((co & GenericParameterAttributes.Contravariant) != 0) return 9;

               // in T (contravariant)
               GenericParameterAttributes contra =
                   typeof(IContra<>).GetGenericArguments()[0].GenericParameterAttributes;
               if ((contra & GenericParameterAttributes.Contravariant) == 0) return 10;
               if ((contra & GenericParameterAttributes.Covariant) != 0) return 11;

               return 0;
           }
       }
   }
   ```

3. **Format** with `nix develop -c dotnet fantomas .` and commit.

## Correctness oracle

- The new test passes on both PawPrint and the real runtime (exit code 0).
  Today it crashes PawPrint at the unimplemented InternalCall dispatch.
- Full pure suite shows no regressions — the new dispatch arm only matches
  `"GetGenericParamProps"`, so it can't steal dispatch from other arms.

## Out of scope

- **IOE for non-parameter types.** `typeof(int).GenericParameterAttributes`
  should throw `InvalidOperationException`, and the IL body correctly
  branches to `throw new InvalidOperationException(SR.Arg_NotGenericParameter)`.
  But constructing that exception walks through ResourceManager, which
  eventually hits the unimplemented `RuntimeMethodHandle::IsCAVisibleFromDecoratedType`
  QCall. The test omits this case.
- **`Type.GenericParameterAttributes` on method generic parameters.** No
  `RuntimeTypeHandleTarget` arm exists for method-generic params yet, so
  `GetToken` would fail before `GetGenericParamProps` is reached. When that
  arm lands, this InternalCall will work without changes.
- **Force-intrinsic removal (γ).** The `track-a-generic-param-6-3-baseType`
  branch implemented `Type.GenericParameterAttributes` via a force-intrinsic
  mechanism. With β in place, the managed IL body runs cleanly through the
  InternalCall chain without any intrinsic override. γ will formally delete
  any remaining force-intrinsic vestiges and bring that branch's test cases
  onto main. Since force-intrinsic was never merged, γ is just "confirm the
  end-to-end path works and add any remaining test coverage."
