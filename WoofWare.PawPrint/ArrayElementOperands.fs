namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// The `array` and `index` operands of an opcode that names one cell of a vector (`ldelem.*`,
/// `stelem.*`, and the token forms `ldelem <T>`, `stelem <T>` and `ldelema <T>`), checked
/// against the array they name.
[<RequireQualifiedAccess>]
type internal ArrayElementOperands =
    /// `index` names a cell of the array at `array`.
    | InRange of array : ManagedHeapAddress * index : int
    /// The array was null, or the index lay outside it, and the corresponding exception has been
    /// raised into the guest. This is the opcode's result: the program counter has deliberately
    /// not been advanced, because exception dispatch needs the faulting instruction's offset.
    | Faulted of IlMachineState * WhatWeDid

[<RequireQualifiedAccess>]
module internal ArrayElementOperands =

    /// The index an array-element opcode popped, at the width ECMA-335 III.4.7-4.9 and
    /// III.4.26-4.27 compare it against the array's length (`native int` or `int32`). A native-int index keeps every one of its bits:
    /// narrowed to 32 bits first, `0x1_0000_0001` would name element 1 of a three-element array
    /// instead of lying outside it.
    let private arrayIndex (index : EvalStackValue) : int64 =
        match index with
        | EvalStackValue.NativeInt src ->
            match src with
            | NativeIntSource.FunctionPointer _
            | NativeIntSource.FieldHandlePtr _
            | NativeIntSource.MethodHandlePtr _
            | NativeIntSource.TypeHandlePtr _
            | NativeIntSource.TypeDescPtr _
            | NativeIntSource.MethodTablePtr _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _
            | NativeIntSource.PerInstInfoPtr _
            | NativeIntSource.PerInstDictPtr _
            | NativeIntSource.GcHandlePtr _
            | NativeIntSource.AssemblyHandle _
            | NativeIntSource.ModuleHandle _
            | NativeIntSource.MetadataImportHandle _
            | NativeIntSource.EventPipeProviderPtr _
            | NativeIntSource.EventPipeEventPtr _
            | NativeIntSource.LowLevelMonitorPtr _
            | NativeIntSource.WaitHandlePtr _
            | NativeIntSource.EvpMdPtr _
            | NativeIntSource.EvpMdCtxPtr _
            | NativeIntSource.AssemblyBinderPtr _
            | NativeIntSource.ManagedPointer _ -> failwith "Refusing to treat a pointer as an array index"
            | NativeIntSource.SyntheticCrossArrayOffset _ ->
                failwith "Refusing to treat a synthetic cross-storage byte offset as an array index"
            | NativeIntSource.OpaqueHashBits bits ->
                // Synthesised pointer-hash bits are deterministic, so an index derived from them
                // is bounds-checked like any other native int rather than refused.
                bits
            | NativeIntSource.Verbatim i -> i
        | EvalStackValue.Int32 int32Source -> Int32Source.value "array index" int32Source |> int64<int32>
        | _ -> failwith $"Invalid array index: %O{index}"

    /// Checks the popped `array` and `index` operands the way ECMA-335 III.4.7-4.9 (`ldelem`,
    /// `ldelem.<type>`, `ldelema`) and III.4.26-4.27 (`stelem`, `stelem.<type>`) order their faults: a null array raises
    /// `NullReferenceException` before the index is looked at, and an index outside the array,
    /// compared at its full native-int width, raises `IndexOutOfRangeException` before anything
    /// about the element type is checked.
    let resolve
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (index : EvalStackValue)
        (arr : EvalStackValue)
        (thread : ThreadId)
        (state : IlMachineState)
        : ArrayElementOperands
        =
        match arr with
        | EvalStackValue.NullObjectRef ->
            IlMachineStateExecution.raiseOpcodeFault loggerFactory baseClassTypes OpcodeFault.NullReference thread state
            |> ArrayElementOperands.Faulted
        | EvalStackValue.ObjectRef arrAddr ->
            let index = arrayIndex index
            let shape = ManagedHeap.getArrayShape arrAddr state.ManagedHeap

            if index < 0L || index >= int64<int32> shape.Length then
                IlMachineStateExecution.raiseOpcodeFault
                    loggerFactory
                    baseClassTypes
                    OpcodeFault.IndexOutOfRange
                    thread
                    state
                |> ArrayElementOperands.Faulted
            else
                // The check just established `0 <= index < Length`, and `Length` is an int32, so
                // narrowing loses nothing.
                ArrayElementOperands.InRange (arrAddr, int32<int64> index)
        | _ -> failwith $"Invalid array operand: %O{arr}"
