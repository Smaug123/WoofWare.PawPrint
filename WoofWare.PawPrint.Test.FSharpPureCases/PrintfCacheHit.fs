module PrintfCacheHit

// FSC compiles an interpolated string with `%` specifiers into a `PrintfFormat`5` allocation
// followed by a call taking the `PrintfFormat`4` base. When the expression is used at type `obj`,
// the two disagree: the `newobj` here is
//
//     newobj PrintfFormat`5<obj, obj, obj, obj, Widget>::.ctor(string, obj[], Type[])
//     call   PrintfModule::PrintFormatToStringThen<obj>(PrintfFormat`4<obj, Unit, string, string>)
//
// so an object whose base is `PrintfFormat`4<obj, obj, obj, obj>` is passed where
// `PrintfFormat`4<obj, Unit, string, string>` is declared. Generic parameters are invariant, so
// that is unverifiable IL — ILVerify reports exactly one error in this assembly, `[StackUnexpected]`
// at that call — and it is a compiler defect, reported as dotnet/fsharp#20270.
//
// Real .NET runs it anyway, because every type argument involved is a reference type and CoreCLR
// shares instance FieldDescs across compatible instantiations ("INSTANCE FIELDDESCS ARE
// REPRESENTATIVES ... SHARED BY COMPATIBLE GENERIC INSTANTIATIONS", methodtable.h:1964). The
// instantiations are still distinct types with distinct MethodTables; it is the field descriptors
// that coincide, so `ldfld` through either one lands at the same offset. PawPrint keys field
// storage on the exact instantiation, so it cannot follow, and it refuses the call rather than
// continuing into a callee whose types it models differently.
//
// The `obj` annotation on `format` is what makes FSC emit the mismatch; annotating `string`
// instead yields a consistent `PrintfFormat`5<string, Unit, string, string, Widget>` and no
// disagreement. It is also why the `#nowarn`s are needed: `string`-to-`obj` at the return position
// is exactly the implicit conversion (FS3388) that produces the divergent instantiation, so
// silencing the warning is part of reproducing the bug rather than incidental tidying.
//
// PawPrint refuses at the first `format` call, so only the real-runtime arm of the parked fixture
// reaches the second. It is kept so that the guest still exercises the cached path through
// `Cache`4.GetParser` on the runtime that can run it.

#nowarn "3388"
#nowarn "3559"

type Widget = | Widget of int

let format (w : Widget) : obj = $"value %O{w} end"

let main (_argv : string array) : int =
    let w1 = Widget 42
    let w2 = Widget 99
    let a = (format w1).ToString ()
    let b = (format w2).ToString ()

    if a <> "value Widget 42 end" then 1
    elif b <> "value Widget 99 end" then 2
    else 0
